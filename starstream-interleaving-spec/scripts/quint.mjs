#!/usr/bin/env node
import { spawnSync } from 'node:child_process'
import { existsSync } from 'node:fs'
import { basename, join } from 'node:path'

const ROOT = join(import.meta.dirname, '..')

const SPEC_DIR = 'spec'
const SPEC = `${SPEC_DIR}/starstream.qnt`
const SIM = `${SPEC_DIR}/sim.qnt`
const MAIN = 'starstream_sim'

const INVARIANTS = [
  'coord_stack',
  'consumed_utxo_stays_consumed',
  'consumed_utxos_have_no_methods',
]

const WITNESSES = [
  'tx_finished',
  'has_bound_resource',
  'has_yielded_utxo',
  'has_dead_utxo',
]

const SIMULATE = [
  'run',
  SIM,
  `--main=${MAIN}`,
  '--invariants', ...INVARIANTS,
  '--witnesses', ...WITNESSES,
  '--max-samples=1000',
  '--max-steps=30',
  '--backend=rust',
  '--verbosity=3',
  '--mbt',
  '--hide', 'prev_lifecycle',
]

const QUIET = ['--verbosity=1']

function groupByFlag(args) {
  const groups = []

  for (const arg of args) {
    const name = arg.startsWith('--') ? arg.slice(2).split('=')[0] : undefined

    if (name === undefined && groups.length > 0) {
      groups.at(-1).args.push(arg)
    } else {
      groups.push({ name, args: [arg] })
    }
  }

  return groups
}

function override(base, extras) {
  const replaced = new Set(groupByFlag(extras).map((group) => group.name))

  return [
    ...groupByFlag(base)
      .filter((group) => group.name === undefined || !replaced.has(group.name))
      .flatMap((group) => group.args),
    ...extras,
  ]
}

const TASKS = {
  typecheck: [
    ['typecheck', SPEC],
    ['typecheck', SIM],
  ],

  simulate: [SIMULATE],

  verify: [['verify', SIM, `--main=${MAIN}`, '--invariants', ...INVARIANTS]],

  repl: [
    {
      cwd: join(ROOT, SPEC_DIR),
      args: ['repl', '-r', `${basename(SIM)}::${MAIN}`, '--seed=0x12060'],
    },
  ],
}

TASKS.check = [...TASKS.typecheck, override(SIMULATE, QUIET)]

const localQuint = join(
  ROOT,
  'node_modules',
  '.bin',
  process.platform === 'win32' ? 'quint.cmd' : 'quint',
)

const quint = existsSync(localQuint) ? localQuint : 'quint'

const [task, ...extraArgs] = process.argv.slice(2)
const commands = Object.hasOwn(TASKS, task) ? TASKS[task] : undefined

if (!commands) {
  console.error(`usage: node scripts/quint.mjs <${Object.keys(TASKS).join('|')}> [quint args...]`)
  process.exit(2)
}

for (const [index, command] of commands.entries()) {
  const { args, cwd = ROOT } = Array.isArray(command) ? { args: command } : command
  const last = index === commands.length - 1

  const { status, error } = spawnSync(quint, last ? override(args, extraArgs) : args, {
    cwd,
    stdio: 'inherit',
    shell: process.platform === 'win32',
  })

  if (error) {
    console.error(`failed to run ${quint}: ${error.message}`)
    console.error('run `npm install` in this crate to provide it')
    process.exit(1)
  }

  if (status !== 0) {
    process.exit(status ?? 1)
  }
}

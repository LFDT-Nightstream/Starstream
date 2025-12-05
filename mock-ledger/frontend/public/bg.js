const debounce = (fn, timeout = 300) => {
  let timer
  return (...args) => {
    clearTimeout(timer)
    timer = setTimeout(() => fn.apply(this, args), timeout)
  }
}

document.addEventListener('DOMContentLoaded', () => {
  const city = document.getElementById('city')
  const dot = document.getElementById('dot')
  const sky = document.getElementById('sky')

  let buildings
  let dimensions
  let highest

  const animate = (frame = 0) => {
    frame % 2 === 0 && newStar()
    frame % 80 === 0 && switchDot()
    switchWindow()
    window.requestAnimationFrame(() => animate(++frame))
  }

  const gauss = number => {
    return (number * (number + 1)) / 2
  }

  const newBuilding = () => {
    const building = document.createElement('div')
    const height = Math.round(Math.floor(Math.random() * (dimensions.city.height - dimensions.city.height / 8)) + dimensions.city.height / 8)
    const width = Math.round(Math.floor(Math.random() * (dimensions.city.height / 4)) + dimensions.city.height / 4)
    const left = Math.round(Math.floor(Math.random() * dimensions.city.width) - width / 2)
    const windowFrame = Math.floor(Math.random() * 3) + 3
    const windowSize = Math.random() * 3 > 1 ? 2 : 1
    building.classList.add('building')
    building.style.height = `${height}px`
    building.style.left = `${left}px`
    building.style.width = `${width}px`
    for (let i = 0; i < Math.floor(height / (windowFrame * 2 + windowSize)); i++) {
      const floor = newFloor(width, windowFrame, windowSize)
      building.appendChild(floor)
    }
    city.appendChild(building)
    if (!highest || height > highest.height) {
      highest = { building, height, left, width }
    }
    return building
  }

  const newFloor = (width, windowFrame, windowSize) => {
    const floor = document.createElement('div')
    floor.className = 'floor'
    floor.style.height = `${windowFrame * 2 + windowSize}px`
    for (let i = 0; i < Math.floor(width / (windowFrame * 2 + windowSize)); i++) {
      const color = ['#999900', '#999966', '#cccc00', '#cccc66', '#eeee00', '#eeee66', '#ffff00'][Math.floor(Math.random() * 7)]
      const window = document.createElement('span')
      window.className = 'window'
      if (windowSize === 1) {
        window.style.backgroundColor = color
      } else {
        window.style.backgroundColor = Math.random() * 4 > 1 ? color : 'black'
        window.style.boxShadow = `1px 0 ${Math.random() * 4 > 1 ? color : 'black'}, 0 1px ${Math.random() * 4 > 1 ? color : 'black'}, 1px 1px ${Math.random() * 4 > 1 ? color : 'black'}`
      }
      window.style.margin = `${windowFrame}px`
      floor.appendChild(window)
    }
    return floor
  }

  const newStar = () => {
    const star = document.createElement('span')
    star.classList.add('star')
    star.style.backgroundColor = ['#666666', '#777777', '#888888', '#999999', '#aaaaaa', '#bbbbbb', '#cccccc', '#dddddd', '#eeeeee', '#ffffff'][Math.floor(Math.random() * 10)]
    star.style.left = `${Math.floor(Math.random() * dimensions.sky.width)}px`
    let guess = Math.floor(Math.random() * gauss(dimensions.sky.height)) + 1
    let i = 0
    while (guess > 0) {
      guess -= ++i
    }
    star.style.top = `${dimensions.sky.height - i}px`
    sky.appendChild(star)
    setTimeout(() => sky.removeChild(star), Math.round(Math.random() * dimensions.sky.height * dimensions.sky.width) + 60000)
  }

  const reset = () => {
    if (Array.isArray(buildings) && buildings.length > 0) {
      buildings.forEach(building => city.removeChild(building))
      highest = { height: 0 }
    }
    buildings = []
    dimensions = { sky: sky.getBoundingClientRect() }
    city.style.height = `${Math.round(Math.floor(Math.random() * (dimensions.sky.height / 3 - dimensions.sky.height / 5)) + dimensions.sky.height / 5)}px`
    dimensions.city = city.getBoundingClientRect()
    for (let i = 0; i < Math.round(dimensions.city.width / (Math.floor(Math.random() * 13) + 8)); i++) {
      buildings.push(newBuilding())
    }
    const diameter = Math.floor(Math.random() * 4) + 5
    dot.style.bottom = `${highest.height + diameter / 2}px`
    dot.style.height = `${diameter}px`
    dot.style.left = `${Math.round(highest.left + highest.width / 2 - diameter / 2)}px`
    dot.style.width = `${diameter}px`
  }

  const switchDot = () => {
    dot.classList.toggle('on')
  }

  const switchWindow = () => {
    const windows = document.querySelectorAll('.window')
    const windowsOn = document.querySelectorAll('.window.on')
    let randomWindow = windows[Math.floor(Math.random() * windows.length)]
    if (windowsOn.length / windows.length < 0.72) {
      while (randomWindow.classList.contains('on')) {
        randomWindow = windows[Math.floor(Math.random() * windows.length)]
      }
    } else {
      while (!randomWindow.classList.contains('on')) {
        randomWindow = windows[Math.floor(Math.random() * windows.length)]
      }
    }
    randomWindow.classList.toggle('on')
  }

  reset()
  window.addEventListener('resize', debounce(reset))
  animate()
})
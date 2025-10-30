// In its own file so the registerVsixExtension() call doesn't get hot-reloaded unnecessarily.
import starstream_vsix from "file-loader!../../vscode-starstream/starstream.vsix";
import { registerVsixExtension } from "./vsix";
export default await registerVsixExtension(starstream_vsix);

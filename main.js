var electron = require("electron")
var cp = require("child_process");

var app = electron.app;
var BrowserWindow = electron.BrowserWindow;
var main = {
  window: null,
  engine: null
};

main.engine = cp.spawn("./dist/build/radler/radler");

function createWindow() {
  main.window = new BrowserWindow({
    width: 1000,
    height: 800
  });

  main.window.loadFile("public/index.html")
  main.window.openDevTools()
  main.window.webContents.on("did-finish-load", function () {

  });

  main.window.on("closed", function () {
    main.window = null
  });
}

app.on("ready", createWindow);

app.on("window-all-closed", function () {
  if (process.platform !== "darwin") {
    app.quit()
  }
});

app.on("activate", function () {
  if (main.window === null) {
    createWindow();
  }
});
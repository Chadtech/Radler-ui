var electron = require("electron")
var cp = require("child_process");

var app = electron.app;
var BrowserWindow = electron.BrowserWindow;

var model = {
  window: null,
  engine: null,
  port: 3000
};

model.engine = cp.spawn("./dist/build/radler/radler");

function createWindow() {
  model.window = new BrowserWindow({
    width: 1000,
    height: 800
  });

  model.window.loadFile("public/index.html");
  model.window.openDevTools();
  model.window.webContents.on("did-finish-load", function () {
    model.window.webContents.send('init', {
      port: model.port
    });
  });

  model.window.on("closed", function () {
    model.window = null
  });
}

app.on("ready", createWindow);

app.on("window-all-closed", function () {
  if (process.platform !== "darwin") {
    app.quit()
  }
});

app.on("activate", function () {
  if (model.window === null) {
    createWindow();
  }
});
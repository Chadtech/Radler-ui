var fs = get('fs');
var electron = get('electron')

var ipcRenderer = electron.ipcRenderer;

function projectDir(path) {
  return './project/' + path;
}

function partsDir(path) {
  if (typeof path === 'undefined') {
    return projectDir('parts');
  }
  return projectDir('parts/' + path);
}

function readPackage() {
  return fs.readFileSync(
    projectDir('package.json'),
    'utf-8'
  )
}

function readParts() {
  return fs.readdirSync(partsDir())
    .map(function (partName) {
      return {
        name: partName,
        data:
          fs.readFileSync(
            partsDir(partName),
            'utf-8'
          )
      };
    })
}

var app = Elm.Main.init({
  flags: {
    package: readPackage(),
    parts: readParts()
  }
});

function toElm(type, payload) {
  app.ports.fromJs.send({
    type: type,
    payload: payload
  });
}

var actions = {
  savePartToDisk: function (payload) {
    fs.writeFileSync(
      partsDir(payload.name),
      payload.data
    );
  },
  savePackageToDisk: function (payload) {
    fs.writeFileSync(
      projectDir('package.json'),
      payload
    );
  },
  saveScoreToDisk: function (payload) {
    fs.writeFileSync(
      projectDir('score'),
      payload
    );
  }
}

function jsMsgHandler(msg) {
  var action = actions[msg.type];
  if (typeof action === "undefined") {
    console.log("Unrecognized js msg type ->", msg.type);
    return;
  }
  action(msg.payload);
}

function engineMsgHandler(msg) {
  console.log("!", msg);
  switch (msg.type) {
    default:
      toElm("Unrecognized engine msg type", msg.type);
  }
}

ipcRenderer.on("from-engine", function (_, msg) {
  engineMsgHandler(msg);
});

app.ports.toJs.subscribe(jsMsgHandler)


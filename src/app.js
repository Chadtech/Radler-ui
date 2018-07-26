var fs = get('fs');

function projectDir(path) {
	return './project/' + path;
}

function partsDir(path) {
	if (typeof path === 'undefined') {
		return projectDir('parts');
	}
	return projectDir('parts/' + path);
}

var app = Elm.Main.init({
	flags: {
		package:
			fs.readFileSync(
				projectDir('package.json'),
				'utf-8'
			),
		sheets:
			fs.readdirSync(partsDir())
				.map(function (sheetName) {
					return {
						name: sheetName,
						data:
							fs.readFileSync(
								partsDir(sheetName),
								'utf-8'
							)
					};
				})
	}
});

function toElm(type, payload) {
	app.ports.fromJs.send({
		type: type,
		payload: payload
	});
}

var actions = {
	saveSheetToDisk: function (payload) {
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

app.ports.toJs.subscribe(jsMsgHandler)


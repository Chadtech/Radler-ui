var fs = get('fs');

var app = Elm.Main.init({
	flags: {
		package: fs.readFileSync(
			'./project/package.json',
			'utf-8'
		)
	}
});

function toElm(type, payload) {
	app.ports.fromJs.send({
		type: type,
		payload: payload
	});
}

var actions = {
	saveSheet: function (payload) {
		console.log(payload);
	},
	savePackage: function (payload) {
		console.log('SAVE PACKAGE', payload);
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


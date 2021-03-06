module.exports = function(app, elm, expressWs) {

    var state = {
        status: 'initialising',
        idCount: 0,
        queue: [],
        messages: []
    };

    function renderState() {
        return JSON.stringify(state)
    }


    app.ws('/dashboard', function(ws, req) {
        ws.send(renderState());
        ws.on('message', function(msg) {
            ws.send(renderState())
        });
    });

    elm.ports.sendState.subscribe(function(stateString) {
        state = JSON.parse(stateString);
        expressWs.getWss().clients.forEach(x => x.send(renderState()))
    })

}

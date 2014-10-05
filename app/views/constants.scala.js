@(constants : play.api.libs.json.JsValue, routes : JavaScript)
app.constant('constantData',@format.raw(constants.toString));
@routes;app.constant('routeData',Object.freeze(routes).controllers);

function $_GET(param) {
	var vars = {};
	window.location.href.replace( location.hash, '' ).replace( 
		/[?&]+([^=&]+)=?([^&]*)?/gi, // regexp
		function( m, key, value ) { // callback
			vars[key] = value !== undefined ? value : '';
		}
	);

	if ( param ) {
		return vars[param] ? vars[param] : null;	
	}
	return vars;
}

var HMUD_Config = {
/*
 * The host hMUD will try to connect, you can use window.location.hostname to
 * make hMUD automatically connect to the same host it's being accessed by the
 * user. Examples:
 *
 * host: "debomud.org",
 * host: window.location.hostname,
 * host: "127.0.0.1",
 host: 'prologmoo.com',
 */
    host: window.location.hostname,
    port: window.location.port?window.location.port:$_GET("port"),
   // port: _GET("port"),

/*
 * The port the Flash Policy Server is listening. You MUST have this correctly
 * configured due to Flash's strict security policies.
 */
    policyPort: 4010,

/* The maximum length of the command history list */
    maxHistorySize: 1000,

/* The minimum length a command should have to be added to the history. */
    historyMinLength: 0,

/*
 * If you scroll up, the scroll will be locked at that position, so that you
 * won't be annoyed by new content being sent. But you might want to
 * automatically scroll down when you type a command. This options does
 * precisely that.
 */
  forceScrollOnCmd: true
};


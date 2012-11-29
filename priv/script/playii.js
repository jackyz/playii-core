if (!this.JSON) {
    JSON = function () {
        function f(n) {    // Format integers to have at least two digits.
            return n < 10 ? '0' + n : n;
        }
        Date.prototype.toJSON = function () {
// Eventually, this method will be based on the date.toISOString method.
            return this.getUTCFullYear()   + '-' +
                 f(this.getUTCMonth() + 1) + '-' +
                 f(this.getUTCDate())      + 'T' +
                 f(this.getUTCHours())     + ':' +
                 f(this.getUTCMinutes())   + ':' +
                 f(this.getUTCSeconds())   + 'Z';
        };
        var m = {    // table of character substitutions
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\f': '\\f',
            '\r': '\\r',
            '"' : '\\"',
            '\\': '\\\\'
        };
        function stringify(value, whitelist) {
            var a,          // The array holding the partial texts.
                i,          // The loop counter.
                k,          // The member key.
                l,          // Length.
                r = /["\\\x00-\x1f\x7f-\x9f]/g,
                v;          // The member value.
            switch (typeof value) {
            case 'string':
// If the string contains no control characters, no quote characters, and no
// backslash characters, then we can safely slap some quotes around it.
// Otherwise we must also replace the offending characters with safe sequences.
                return r.test(value) ?
                    '"' + value.replace(r, function (a) {
                        var c = m[a];
                        if (c) {
                            return c;
                        }
                        c = a.charCodeAt();
                        return '\\u00' + Math.floor(c / 16).toString(16) +
                                                   (c % 16).toString(16);
                    }) + '"' :
                    '"' + value + '"';
            case 'number':
// JSON numbers must be finite. Encode non-finite numbers as null.
                return isFinite(value) ? String(value) : 'null';
            case 'boolean':
            case 'null':
                return String(value);
            case 'object':
// Due to a specification blunder in ECMAScript,
// typeof null is 'object', so watch out for that case.
                if (!value) {
                    return 'null';
                }
// If the object has a toJSON method, call it, and stringify the result.
                if (typeof value.toJSON === 'function') {
                    return stringify(value.toJSON());
                }
                a = [];
                if (typeof value.length === 'number' &&
                        !(value.propertyIsEnumerable('length'))) {
// The object is an array. Stringify every element. Use null as a placeholder
// for non-JSON values.
                    l = value.length;
                    for (i = 0; i < l; i += 1) {
                        a.push(stringify(value[i], whitelist) || 'null');
                    }
// Join all of the elements together and wrap them in brackets.
                    return '[' + a.join(',') + ']';
                }
                if (whitelist) {
// If a whitelist (array of keys) is provided, use it to select the components
// of the object.
                    l = whitelist.length;
                    for (i = 0; i < l; i += 1) {
                        k = whitelist[i];
                        if (typeof k === 'string') {
                            v = stringify(value[k], whitelist);
                            if (v) {
                                a.push(stringify(k) + ':' + v);
                            }
                        }
                    }
                } else {
// Otherwise, iterate through all of the keys in the object.
                    for (k in value) {
                        if (typeof k === 'string') {
                            v = stringify(value[k], whitelist);
                            if (v) {
                                a.push(stringify(k) + ':' + v);
                            }
                        }
                    }
                }
// Join all of the member texts together and wrap them in braces.
                return '{' + a.join(',') + '}';
            }
        }
        return {
            stringify: stringify,
            parse: function (text, filter) {
                var j;
                function walk(k, v) {
                    var i, n;
                    if (v && typeof v === 'object') {
                        for (i in v) {
                            if (Object.prototype.hasOwnProperty.apply(v, [i])) {
                                n = walk(i, v[i]);
                                if (n !== undefined) {
                                    v[i] = n;
                                } else {
                                    delete v[i];
                                }
                            }
                        }
                    }
                    return filter(k, v);
                }
// Parsing happens in three stages. In the first stage, we run the text against
// regular expressions that look for non-JSON patterns. We are especially
// concerned with '()' and 'new' because they can cause invocation, and '='
// because it can cause mutation. But just to be safe, we want to reject all
// unexpected forms.

// We split the first stage into 4 regexp operations in order to work around
// crippling inefficiencies in IE's and Safari's regexp engines. First we
// replace all backslash pairs with '@' (a non-JSON character). Second, we
// replace all simple value tokens with ']' characters. Third, we delete all
// open brackets that follow a colon or comma or that begin the text. Finally,
// we look to see that the remaining characters are only whitespace or ']' or
// ',' or ':' or '{' or '}'. If that is so, then the text is safe for eval.

                if (/^[\],:{}\s]*$/.test(text.replace(/\\./g, '@').
replace(/"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g, ']').
replace(/(?:^|:|,)(?:\s*\[)+/g, ''))) {
// In the second stage we use the eval function to compile the text into a
// JavaScript structure. The '{' operator is subject to a syntactic ambiguity
// in JavaScript: it can begin a block or an object literal. We wrap the text
// in parens to eliminate the ambiguity.
                    j = eval('(' + text + ')');
// In the optional third stage, we recursively walk the new structure, passing
// each name/value pair to a filter function for possible transformation.
                    return typeof filter === 'function' ? walk('', j) : j;
                }
// If the text is not JSON parseable, then a SyntaxError is thrown.
                throw new SyntaxError('parseJSON');
            }
        };
    }();
}

/*
%% ENGX : the Javascript evaluate engine
%%        communicate with erlang process via port protocol
%%        provide the javascript evaluate envirment
%%
%%  port protocl
%%  load
%%    -->
%%      "[load, code, source]\n"
%%    <--
%%      "[load, 1, command, log]\n"
%%      "[load, 0, error]\n"
%%  init
%%    -->
%%      "[init, code, args]\n"
%%    <--
%%      "[init, 1, state0, command, log]\n"
%%      "[init, 0, error]\n"
%%  exec
%%    -->
%%      "[exec, code, state1, func, args]\n"
%%    <--
%%      "[exec, 1, state2, command, log]\n"
%%      "[exec, 0, error]\n"
%%  any
%%    -->
%%      "...\n"
%%    <--
%%      "[any, 0, error]\n"
*/

if(!this.ENGX) {
  ENGX = function(){

    // the script map (keep via instance lifetime)
    var smap = {};
    // the runtime context (keep via instance lifetime)
    var runtime = evalcx('');
    // the log buffer (keep via function call)
    var logbuff = [];
    // the cmd buffer (keep via function call)
    var cmdbuff = [];

    // the clone function
    function clone(to, from){
      for(var f in from){
	if (typeof from[f] !== "function") continue;
	to[f] = from[f];
      }
      return to;
    }

    // the cmdbuff operate function
    function cmd(cmd, pn, func, args){
      // log("DEBUG", "cmd("+cmd+","+pn+","+func+","+args+")");
      if (typeof pn == "string") {
	cmdbuff.push([cmd, pn, func, args]);
      } // todo process when the pn is array
    }

    // the logbuff operate function
    function log(level, str){
      var d = new Date();
      var s = "["+
	(d.getFullYear()) +'.'+
	(d.getMonth() + 1) +'.'+
	(d.getDate()) +'-'+
	(d.getHours()) +':'+
	(d.getMinutes()) +':'+
	(d.getSeconds()) +
	"] "+ level +": "+ str;
      logbuff.push(s);
    }

    // spawn
    function spawn(where){
      var w, a;
      w = where;
      function args(){
	a = [];
	for(var i=0; i<arguments.length; i++) a.push(arguments[i]);
	cmdbuff.push(["spawn", w, a]);
      }
      return args;
    }

    // async
    function async(where){
      var w, f, a;
      w = where;
      function args(){
	a = [];
	for(var i=0; i<arguments.length; i++) a.push(arguments[i]);
	cmdbuff.push(["async", w, f, a]);
      }
      function func(fun){
	f = fun;
	return args;
      }
      return func;
    }

    // cast
    function cast(who){
      var w, f, a;
      w = who;
      function args(){
	a = [];
	for(var i=0; i<arguments.length; i++) a.push(arguments[i]);
	cmdbuff.push(["cast", w, f, a]);
      }
      function func(fun){
	f = fun;
	return args;
      }
      return func;
    }

    // endup
    function endup(){
      cmdbuff.push(["endup"]);
    }

    // **** constuct the runtime
    (function(){
       // core function
       runtime.mixin    = function(d,c){ return clone(d,c); };
       // proc function
       runtime.spawn    = function(w){ return spawn(w);     };
       runtime.async    = function(w){ return async(w);     };
       runtime.endup    = function(){  return endup();      };
       // msg function
       runtime.cast     = function(w){ return cast(w);      };
       // log function
       runtime.debug    = function(s){ return log("DEBUG", s); };
       runtime.warn     = function(s){ return log("WARN ", s); };
       runtime.info     = function(s){ return log("INFO ", s); };
       runtime.error    = function(s){ return log("ERROR", s); };
       runtime.fatal    = function(s){ return log("FATAL", s); };
       // json function
       runtime.dumps    = function(o){ return JSON.stringify(o); };
       runtime.loads    = function(s){ return JSON.parse(s); };
       // seal runtime
       seal(runtime);
     })();

    function load(script, source) {
      if (typeof(script) !== "string") throw "script:"+script+" not string";
      if (typeof(source) !== "string") throw "source:"+source+" not string";
      //// eval source on run-ctx to create the obj
      var obj = evalcx(source, runtime);
      //// check the obj, obj must be a function
      if (typeof(obj) !== "function") throw "source: not function";
      //// check the obj, all properties of obj must be a function too
      var fc = 0;
      for(var n in obj){
	if (n == 'prototype') continue;
	if (typeof(obj[n]) !== 'function') throw "source: ."+n+" not function";
	else fc++;
      }
      if (fc == 0) throw "source: has no property function";
      //print(obj.toSource());
      //// store the jsobject
      smap[script] = obj;
      return obj;
    }

    function init(script, args) {
      if (typeof(script) !== "string") throw "script:"+script+" not string";
      if (! args instanceof Array) throw "args:"+args.toSource()+" not array";
      //// check script
      if (! script in smap) throw "script:"+script+" not load";
      //// construnct instance
      var obj = {};
      smap[script].apply(obj, args);
      return obj;
    }

    function exec(script, sJson, func, args) {
      if (typeof(script) !== "string") throw "code name:"+script+" not string";
      if (typeof(sJson) !== "string") throw "state json:"+sJson+" not string";
      if (typeof(func) !== "string") throw "func name:"+func+" not string";
      if (! args instanceof Array) throw "args:"+args.toSource()+" not array";
      //// check script
      if (! script in smap) throw "script:"+script+" not load";
      //// check script func function
      if (typeof(smap[script][func]) !== "function") throw "func: "+script+"."+func+" not define";
      //// retrieve instance
      var obj = rehydra(sJson);
      //// check instance
      if (typeof(obj) !== "object") throw "data:"+obj+" not object";
      //// clone functions from prototype
      //// but doesn't work for closure functions
      // clone(obj, smap[script]);
      // obj[func].apply(obj, args);
      //// perform
      smap[script][func].apply(obj, args);
      return obj;
    }

    function rehydra(str) {
      // return evalcx(str, runtime);
      return JSON.parse(str);
    }

    function dehydra(obj) {
      // return obj.toSource();
      return JSON.stringify(obj);
    }

    function loop() {
      // main loop
      var l = null;
      var i = 0;
      while (l = readline()) {
        // print("line:"+(++i));
        var a = evalcx(l, runtime);
        var c = a[0];
        var r = null;
        logbuff = [];
        cmdbuff = [];
        try {
          switch (c) {
            case "load": // load(script, source);
	      load(a[1], a[2]);
              r = [c, 1, dehydra(cmdbuff), dehydra(logbuff)];
              break;
            case "init": // init(script, args);
              r = [c, 1, dehydra(init(a[1], a[2])), dehydra(cmdbuff), dehydra(logbuff)];
              break;
            case "exec": // exec(script, data, func, args);
              r = [c, 1, dehydra(exec(a[1], a[2], a[3], a[4])), dehydra(cmdbuff), dehydra(logbuff)];
              break;
            default:
              r = ["any", 0, "command:"+c+" unknown"];
          }
        } catch(e) {
          r = [c, 0, e];
        } finally {
          print(JSON.stringify(r));
          cmdbuff = null;
          logbuff = null;
          r = null;
          c = null;
          a = null;
          l = null;
          gc();
        }
      }
    }
    // EXPORT for OUTSIDE
    return {loop:loop};
  }();

}

ENGX.loop();


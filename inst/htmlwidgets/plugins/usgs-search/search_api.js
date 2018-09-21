/*
===========================================================
search_api.js (version 2.1)
-----------------------------------------------------------
JavaScript API for creating a location search widget in a web page.
The primary data source is a dedicated database and data service created for this api.
The ESRI Online Geocoding Service ( http://geocode.arcgis.com/arcgis/ ) is used as a secondary data source when no api service results found.
-----------------------------------------------------------
This API requires jQuery ( https://jquery.com/ ) to be loaded.
-----------------------------------------------------------
U.S. Geological Survey, Texas Water Science Center, Austin
https://txpub.usgs.gov/DSS/search_api/
===========================================================
*/
//"use strict"; // dev

// create api object
if (window.search_api === undefined) { window.search_api = {}; }


//===========================================================
// PUBLIC PROPERTIES (documented)
//===========================================================

//..................
// user changeable

// whether to write api status messages to developer console
search_api.verbose = false;

//..................
// read only

// api version number
search_api.version = "2.1";


//===========================================================
// PRIVATE PROPERTIES (not documented)
//===========================================================

// define console if needed
if (window.console === undefined) {
    window.console = {
        log   : function(){},
        warn  : function(){},
        error : function(){},
        clear : function(){}
    };
}

// jquery must be loaded
search_api._isLoadedJq = (window.jQuery !== undefined ? true : false);
if (!search_api._isLoadedJq) { console.error("The search_api requires jQuery. The search_api will not function properly."); }

// caller for database logging - reset this for custom usage logging for specific applications
search_api._caller = "jsapi_2.1";

// base url of primary api data service for suggestion menu
search_api._apiServiceUrl = "https://txpub.usgs.gov/DSS/search_api/2.1/services/services.ashx/search";

// base url of secondary geocoding service
search_api._2ndServiceUrl = "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates";

// array of allowed result types for secondary search service (those not in list are rejected)
search_api._2ndOkTypes = [
    // ...like GNIS feature classes primary search uses...
    "STATE OR PROVINCE",
    "COUNTY",
    "CITY",
    "OTHER POPULATED PLACE",
    "STATE CAPITAL",
    "NATIONAL CAPITAL",
    "STREAM",
    "CANAL",
    "CHANNEL",
    "LAKE",
    "RESERVOIR",
    "CAPE",
    "BAY",
    "BASIN",
    "DAM",
    "WATERFALL",
    "MARINA",
    "OTHER WATER FEATURE",
    "SPRING",
    "PARK",
    // ...not like GNIS but allowed...
    "MUSEUM",
    "HOSPITAL",
    "SCHOOL",
    "HISTORICAL MONUMENT",
    "RUIN",
    "AIRPORT",
    "SPORTS CENTER",
    "TOURIST ATTRACTION",
    "AMUSEMENT PARK",
    "MILITARY BASE",
    "POLICE STATION",
    // ...street addresses have type ""...
    ""
    // ...more...
];

// long state name to abbreviation lookup for secondary service results
search_api._statesLong2short = {
    "ALASKA"               : "AK",
    "HAWAII"               : "HI",
    "PUERTO RICO"          : "PR",
    "ALABAMA"              : "AL",
    "ARKANSAS"             : "AR",
    "ARIZONA"              : "AZ",
    "CALIFORNIA"           : "CA",
    "COLORADO"             : "CO",
    "CONNECTICUT"          : "CT",
    "WASHINGTON, D.C."     : "DC",
    "DISTRICT OF COLUMBIA" : "DC",
    "DELAWARE"             : "DE",
    "FLORIDA"              : "FL",
    "GEORGIA"              : "GA",
    "IOWA"                 : "IA",
    "IDAHO"                : "ID",
    "ILLINOIS"             : "IL",
    "INDIANA"              : "IN",
    "KANSAS"               : "KS",
    "KENTUCKY"             : "KY",
    "LOUISIANA"            : "LA",
    "MASSACHUSETTS"        : "MA",
    "MARYLAND"             : "MD",
    "MAINE"                : "ME",
    "MICHIGAN"             : "MI",
    "MINNESOTA"            : "MN",
    "MISSOURI"             : "MO",
    "MISSISSIPPI"          : "MS",
    "MONTANA"              : "MT",
    "NORTH CAROLINA"       : "NC",
    "NORTH DAKOTA"         : "ND",
    "NEBRASKA"             : "NE",
    "NEW HAMPSHIRE"        : "NH",
    "NEW JERSEY"           : "NJ",
    "NEW MEXICO"           : "NM",
    "NEVADA"               : "NV",
    "NEW YORK"             : "NY",
    "OHIO"                 : "OH",
    "OKLAHOMA"             : "OK",
    "OREGON"               : "OR",
    "PENNSYLVANIA"         : "PA",
    "RHODE ISLAND"         : "RI",
    "SOUTH CAROLINA"       : "SC",
    "SOUTH DAKOTA"         : "SD",
    "TENNESSEE"            : "TN",
    "TEXAS"                : "TX",
    "UTAH"                 : "UT",
    "VIRGINIA"             : "VA",
    "VIRGIN ISLANDS"       : "VI",
    "VERMONT"              : "VT",
    "WASHINGTON"           : "WA",
    "WISCONSIN"            : "WI",
    "WEST VIRGINIA"        : "WV",
    "WYOMING"              : "WY",
    ""                     : "N/A"
};

// list of all valid 2-character state abbreviations
search_api._stateAbbrevs = [];
if (search_api._isLoadedJq) {
    $.each( search_api._statesLong2short, function(l,s){
        search_api._stateAbbrevs.push(s);
    });
}

// default options
//   defaults are also used for user input validation
//   all options must be defined here with api default values
//   all names must be lowercase for case insensitivity of user's input
search_api._opt_default = {
    
    //......................
    // appearance
    //......................
    
    // widget sizing option, one of:
    //   "lg" - large size
    //   "md" - medium size (default)
    //   "sm" - small size
    //   "xs" - extra small size
    size : "md",
    
    // width of the widget [px]
    // omit or set 0 to use a predetermined width based on the "size" option
    width : 0,
    
    // text box placeholder prompt to display when no text is entered
    // not supported in IE9 and below
    placeholder : "Search for a place",
    
    // text box hover tooltip
    tooltip : "",
    
    //......................
    // search area
    //......................
    
    // restrict search by geographic latitude and/or longitude (decimal degrees)
    lat_min :  -90,
    lat_max :   90,
    lon_min : -180,
    lon_max :  180,
    
    // restrict search to 1 or more U.S. States or Territories
    // csv-list of 1 or more 2-character State or Territory codes
    // set "all" to search all States and Territories
    search_states : "all",
    
    //......................
    // suggestion menu
    //......................
    
    // minimum number of characters required before attempting to find menu suggestions
    menu_min_char : 2,
    
    // maximum number of menu items to display
    menu_max_entries : 50,
    
    // maximum menu height [px]
    // omit or set 0 to use the maximum available space
    // if a number larger than the available space is specified, the available space is used
    menu_height : 0,
    
    // whether to include GNIS places as suggestions in the menu
    include_gnis_major : true,  // major (most common) categories
    include_gnis_minor : false, // minor (less common) categories
    
    // whether to include U.S. States and Territories as suggestions in the menu
    include_state : true,
    
    // whether to include 5-digit zip codes as suggestions in the menu
    include_zip_code : true,
    
    // whether to include 3-digit area codes as suggestions in the menu
    include_area_code : false,
    
    // whether to include USGS site numbers as suggestions in the menu
    include_usgs_sw : false, // surface water
    include_usgs_gw : false, // ground water
    include_usgs_sp : false, // spring
    include_usgs_at : false, // atmospheric
    include_usgs_ot : false, // other
    
    // whether to include Hydrologic Unit Code (HUC) numbers as suggestions in the menu
    include_huc2  : false, //  2-digit
    include_huc4  : false, //  4-digit
    include_huc6  : false, //  6-digit
    include_huc8  : false, //  8-digit
    include_huc10 : false, // 10-digit
    include_huc12 : false, // 12-digit
    
    //......................
    // event callback functions
    //......................
    
    // function to execute when a search is started
    // triggered when the search textbox text changes
    on_search: function(){},
    
    // function to execute when the suggestion menu is updated
    // triggered when new items are displayed in the menu and when the menu closes
    on_update: function(){},
    
    // function to execute when a suggestion is chosen
    // triggered when a menu item is selected
    on_result: function(){},
    
    // function to execute when no suggestions are found for the typed text
    // triggered when services return no results or time out
    on_failure: function(){},
    
    //......................
    // misc
    //......................
    
    // whether to set verbose mode on (true) or off (false)
    verbose : false
};


//===========================================================
// PUBLIC METHODS (documented)
//===========================================================

//-----------------------------------------------------------
// create
//   main method to create a new search widget in a html element
//
// input:
//   id   (REQUIRED, string) - id of the html element, eg: <div id="mySearch"></div>
//   opts (optional, object) - options, see _opt_default for all available
//
// output:
//   widget object upon success
//   undefined upon failure
search_api.create = function( id, opts ) {
    var funcName = "[create]: ";
    
    // jquery must be loaded
    if (!search_api._isLoadedJq) {
        search_api._console(funcName+"The search_api requires jQuery - widget not created","warn");
        return undefined;
    }
    
    // check input
    // ...id...
    if ( id === undefined       ) { search_api._console(funcName+"id of element to create widget in must be input - widget not created","warn"); return undefined; }
    if ( typeof id !== "string" ) { search_api._console(funcName+"input id must be a string - widget not created",                      "warn"); return undefined; }
    if ( $("#"+id).length !== 1 ) { search_api._console(funcName+"unique element with id '"+id+"' does not exist - widget not created", "warn"); return undefined; }
    // ...opts...
    if (opts === undefined) {
        opts = {};
    }
    
    // create widget object with id
    var o = { id: "#"+id };
    
    // add widget in specified html element
    $(o.id)
        .empty()
        .html(
            '<div class="search-api-container">' +
                // search icon
                '<span class="search-api-icon">' +
                    '<svg focusable="false" xmlns="http://www.w3.org/2000/svg">' +
                        '<path d="M15.5 14h-.79l-.28-.27C15.41 12.59 16 11.11 16 9.5 16 5.91 13.09 3 9.5 3S3 5.91 3 9.5 5.91 16 9.5 16c1.61 0 3.09-.59 4.23-1.57l.27.28v.79l5 4.99L20.49 19l-4.99-5zm-6 0C7.01 14 5 11.99 5 9.5S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z"></path>' +
                    '</svg>' +
                '</span>' +
                // text box
                '<span>' +
                    '<input type="text" value=""/>' +
                '</span>' +
                // spinner
                '<span class="search-api-spinner search-api-spinner-hidden"></span>' +
            '</div>'
        );
    
    // add suggestion menu to doc
    o._menu = {
        container : $('<div class="search-api-menu" data-id="'+o.id+'"></div>').appendTo("body")
    };
    
    //................
    // private methods
    
    // return selector for container
    o._getContainer = function() {
        return $(this.id).find(".search-api-container");
    };
    
    // return selector for icon
    o._getIcon = function() {
        return $(this.id).find(".search-api-icon");
    };
    
    // return selector for text box
    o._getTextbox = function() {
        return $(this.id).find("input");
    };
    
    // return selector for spinner
    o._getSpinner = function() {
        return $(this.id).find(".search-api-spinner");
    };
    
    // return selector for menu
    o._getMenu = function() { return o._menu.container; }
    
    // return filtered text box search 'term' and 'states' to use
    o._getSearchTermStates = function() {
        
        // filter term
        var term = $.trim( o._getTextbox().val() ).toUpperCase()
            // ...boiler plate...
            .replace(/[^\000-\177]/g,"") // remove non-ascii     (\000-\177 octal)
            .replace(/[\000-\037]/g, "") // remove non-printable (\000-\037 octal)
            .replace(/[\/\\^$+?()|[\]{}%]/g, "" ) // remove special chars except "*" wild cards and "-","." for lat-lon
            .replace(/,+/g, " ")         // multiple commas with 1 space
            .replace(/\s+/g," ")         // multiple spaces with 1 space
            // ...special terms...
            .replace(/^ST /,"SAINT ");   // "st " => "saint "
        
        // parse any 2-char state abbreviation at end
        var states = "";
        var parts = term.split(" ");
        if ( parts.length>=2 && $.inArray( parts[parts.length-1], search_api._stateAbbrevs )>=0 ) {
            states = parts.pop();
            term   = parts.join(" ");
        }
        
        // compare any parsed state to allowed search_states
        if (states !== "" && o.opts.search_states === "ALL") {
            // have state and all states allowed
            // return term and states as is
        } else if (states !== "" && o.opts.search_states !== "ALL") {
            // have state and specific states allowed
            if ( $.inArray( states, o.opts.search_states.split(",") ) <= -1 ) {
                // not in allowed list
                term   = term + " " + states;  // add parsed state back in term so no match found
                states = o.opts.search_states; // set states to search_states
            }
        } else {
            // no state parsed - set state to search_states
            states = o.opts.search_states;
        }
        
        // return results
        return { term:term, states:states };
    };
    
    //................
    // public methods
    
    // setOpts
    //   set widget options
    //   multiple options can be input using the syntax: setOpts({ opt1:val1, opt2:val2, ... })
    //   a single option  can be input using the syntax: setOpts( "opt1", val1 )
    //
    //   new input options overwrite any previously set options
    //   options not input are kept at the previously set options
    //   options not input or previously set are set to api defaults
    //
    //   any invalid or unrecognized input options are ignored with warning
    //   can be called at any time to update the widget
    //   returns the widget object
    o.setOpts = function(arg1,arg2) {
        var funcName = "[setOpts "+this.id+"]: ";
        
        // handle input syntax: (obj) or (name,val) pair
        var opts = arg1;
        if (arguments.length===2 && typeof arguments[0]==="string") {
            opts = {};
            opts[arguments[0]] = arguments[1];
        }
        
        // opts must be object
        if (typeof opts !== "object") {
            search_api._console(funcName+"input options must be a object of option values - options not updated","warn");
            return this;
        }
        
        // lowercase all input opts for case insensitivity
        $.each( opts, function(name,val) {
            delete opts[name];
            opts[name.toLowerCase()] = val;
        });
        
        // set default or new input opt
        var nWarn = 0;
        var obj = this;
        if (obj.opts === undefined) { obj.opts = {}; }
        $.each( search_api._opt_default, function(name,val_default) {
            if ( obj.opts[name] === undefined ) {
                // not set yet - set default
                obj.opts[name] = val_default;
            }
            if ( opts[name] !== undefined ) {
                // input specified
                if (typeof val_default === "number"  && !isNaN(parseFloat(opts[name]))                    ) { opts[name] = parseFloat(opts[name]); } // support "123xxxx" conversion for number types
                if (typeof val_default === "boolean" && ( opts[name]===1 || /^\s*T/i.test(opts[name]+"") )) { opts[name] = true;                   } // support 1 or "t..." as true  for bool   types
                if (typeof val_default === "boolean" && ( opts[name]===0 || /^\s*F/i.test(opts[name]+"") )) { opts[name] = false;                  } // support 0 or "f..." as false for bool   types
                // check type
                if (typeof opts[name] === typeof val_default) {
                    // type matches - set
                    obj.opts[name] = opts[name];
                } else {
                    // bad type - warn and ignore
                    search_api._console(funcName+"'"+name+"' option value must be a "+(typeof val_default)+" - option ignored","warn");
                    nWarn++;
                }
            }
        });
        
        // give warnings for unrecognized opts
        $.each( opts, function(name) {
            if ( search_api._opt_default[name] === undefined ) {
                search_api._console(funcName+"unrecognized option ignored: "+name,"warn");
                nWarn++;
            }
        });
        
        // data types already checked
        // check values - reset to default if problem
        if ( o.opts.lat_min          <  -90 || o.opts.lat_min >  90 ) { search_api._console(funcName+"invalid 'lat_min' option ("+o.opts.lat_min+"): value must be between -90 and 90 degrees, inclusive - default used",         "warn");  nWarn++;  o.opts.lat_min          = search_api._opt_default.lat_min;          }
        if ( o.opts.lat_max          <  -90 || o.opts.lat_max >  90 ) { search_api._console(funcName+"invalid 'lat_max' option ("+o.opts.lat_max+"): value must be between -90 and 90 degrees, inclusive - default used",         "warn");  nWarn++;  o.opts.lat_max          = search_api._opt_default.lat_max;          }
        if ( o.opts.lon_min          < -180 || o.opts.lon_min > 180 ) { search_api._console(funcName+"invalid 'lon_min' option ("+o.opts.lon_min+"): value must be between -180 and 180 degrees, inclusive - default used",       "warn");  nWarn++;  o.opts.lon_min          = search_api._opt_default.lon_min;          }
        if ( o.opts.lon_max          < -180 || o.opts.lon_max > 180 ) { search_api._console(funcName+"invalid 'lon_max' option ("+o.opts.lon_max+"): value must be between -180 and 180 degrees, inclusive - default used",       "warn");  nWarn++;  o.opts.lon_max          = search_api._opt_default.lon_max;          }
        if ( o.opts.lat_min          > o.opts.lat_max               ) { search_api._console(funcName+"'lat_min' option must be less than 'lat_max' - defaults for both used",                                                     "warn");  nWarn++;  o.opts.lat_min          = search_api._opt_default.lat_min;  o.opts.lat_max = search_api._opt_default.lat_max; }
        if ( o.opts.lon_min          > o.opts.lon_max               ) { search_api._console(funcName+"'lon_min' option must be less than 'lon_max' - defaults for both used",                                                     "warn");  nWarn++;  o.opts.lon_min          = search_api._opt_default.lon_min;  o.opts.lon_max = search_api._opt_default.lon_max; }
        if ( o.opts.menu_min_char    <    1                         ) { search_api._console(funcName+"invalid 'menu_min_char' option ("+o.opts.menu_min_char+"): value must be at least 1 - default used",                        "warn");  nWarn++;  o.opts.menu_min_char    = search_api._opt_default.menu_min_char;    }
        if ( o.opts.menu_max_entries <    1                         ) { search_api._console(funcName+"invalid 'menu_max_entries' option ("+o.opts.menu_max_entries+"): value must be at least 1 - default used",                  "warn");  nWarn++;  o.opts.menu_max_entries = search_api._opt_default.menu_max_entries; }
        if ( o.opts.menu_height      <    0                         ) { search_api._console(funcName+"invalid 'menu_height' option ("+o.opts.menu_height+"): value must be greater or equal to 0 - default used",                 "warn");  nWarn++;  o.opts.menu_height      = search_api._opt_default.menu_height;      }
        if ( $.inArray(o.opts.size,["lg","md","sm","xs"]) <= -1     ) { search_api._console(funcName+"invalid 'size' option ('"+o.opts.size+"'): string must be one of 'lg','md','sm', or 'xs' - default used",                   "warn");  nWarn++;  o.opts.size             = search_api._opt_default.size;             }
        if ( o.opts.width            <    0                         ) { search_api._console(funcName+"invalid 'width' option ("+o.opts.width+"): value must be greater or equal to 0 - default used",                             "warn");  nWarn++;  o.opts.width            = search_api._opt_default.width;            }
        
        // search_states check is more elaborate
        o.opts.search_states = o.opts.search_states.replace(/\s+/g,"").toUpperCase();
        $.each( o.opts.search_states.split(","), function(idx,state) {
            if ( $.inArray( state, search_api._stateAbbrevs.concat("ALL") )<=-1 ) {
                // invalid state
                o.opts.search_states = search_api._opt_default.search_states.toUpperCase();
                search_api._console(funcName+"1 or more 'search_states' is not a valid 2-character State or Territory abbreviation - default used", "warn");
                nWarn++;
                return false; // stop checking
            }
        });
        
        // set global verbose mode if input and remove from widget opts
        if (opts.verbose !== undefined) { search_api.verbose = opts.verbose; }
        delete o.opts.verbose;
        
        // setup widget with options and return widget obj
        search_api._console(funcName+"options set "+(nWarn>0 ? "with "+nWarn+" warnings" : "ok"));
        return search_api._setup(this);
    };
    
    // on
    //   alternate method to set event callback function
    //   'name' is the event name with or without the "on_" prefix, eg: "on_result" or "result"
    //   'func' is this function to connect to the event
    //   optional 3rd argument 'append' appends to any existing callback (true) or overwrites (false, default)
    //   disconnect the event by setting 'func' null ('append' ignored)
    //   returns the widget object
    o.on = function(name,func,append) {
        name = name.toLowerCase(); if (!/^on_/.test(name)) { name = "on_"+name; }
        if (append === undefined) { append = false; }
        if (func===null) {
            // disconnect
            func = function(){};
        } else if (append) {
            // append
            var old_func = this.opts[name];
            var new_func = func;
            func = function() {
                old_func(o);
                new_func(o);
            };
        }
        // use setOpts method to set
        var temp = {}; temp[name] = func;
        return this.setOpts(temp);
    };
    
    // trigger
    //   manually trigger one or more event callbacks
    //   'names' are the event name(s) with or without the "on_" prefix, eg: "on_result" or "result"
    //   use spaces or commas to separate multiple event names
    o.trigger = function(names) {
        var funcName = "[trigger "+this.id+"]: ";
        $.each( names.replace(/,+/g," ").replace(/\s+/g," ").toLowerCase().split(" "), function(idx,name) {
            name = name.replace(/^on_/,"");
            if ( $.inArray(name,["search","update","result","failure"])>=0 ) {
                o.opts["on_"+name](o); // valid event name - execute
                search_api._console(funcName+name+" event triggered");
            } else {
                search_api._console(funcName+"invalid event '"+name+"'","warn");
            }
        });
        return this;
    };
    
    // visible
    //   set visibility of the widget
    //   input true  - show
    //   input false - hide
    //   no input    - toggle 
    //   returns the widget object
    o.visible = function(bool) {
        var funcName = "[visible "+this.id+"]: ";
        switch(bool) { // set class
            case true      : this._getContainer().add( this._getMenu() ).removeClass( "search-api-hidden"); break; // show
            case false     : this._getContainer().add( this._getMenu() ).addClass(    "search-api-hidden"); break; // hide
            case undefined : this._getContainer().add( this._getMenu() ).toggleClass( "search-api-hidden"); break; // toggle
            default: search_api._console(funcName+"invalid input","warn"); return this; // invalid
        }
        search_api._console(funcName+this.isVisible());
        return this;
    };
    
    // isVisible
    //   return the widget's current visible state (true or false)
    o.isVisible = function() {
        return ! this._getContainer().is(".search-api-hidden");
    };
    
    // enable
    //   set the enable state of the widget
    //   input true  - enable
    //   input false - disable
    //   no input    - toggle 
    //   returns the widget object
    o.enable = function(bool) {
        var funcName = "[enable "+this.id+"]: ";
        switch(bool) { // set class
            case true      : this._getContainer().add( this._getMenu() ).removeClass( "search-api-disabled"); break; // enable
            case false     : this._getContainer().add( this._getMenu() ).addClass(    "search-api-disabled"); break; // disable
            case undefined : this._getContainer().add( this._getMenu() ).toggleClass( "search-api-disabled"); break; // toggle
            default: search_api._console(funcName+"invalid input","warn"); return this; // invalid
        }
        this._getTextbox().prop("disabled",!this.isEnabled()); // enable-disable text box
        search_api._console(funcName+this.isEnabled());
        return this;
    };
    
    // isEnabled
    //   return the widget's current enable state (true or false)
    o.isEnabled = function() {
        return ! this._getContainer().is(".search-api-disabled");
    };
    
    // val
    //   set or get the currently entered text box string
    //   input a string to set (no events are triggered)
    //   input nothing  to get
    o.val = function(str) {
        var funcName = "[val "+this.id+"]: ";
        switch(typeof str) {
            case "string"    : this._getTextbox().val(str);                return this; break; // set
            case "undefined" : str = this._getTextbox().val();             return str;  break; // get
            default: search_api._console(funcName+"invalid input","warn"); return this; break; // invalid
        }
    };
    
    // getSuggestions
    //   return geojson feature collection object of all currently suggested menu items
    //   location information for each feature are set as properties
    //   returns empty geojson feature collection when the menu is not open
    o.getSuggestions = function() {
        // set empty
        var geojson = { type:"FeatureCollection", features:[] };
        // add faeture for each menu item
        $.each( o._getMenu().find(".search-api-menu-item"), function(idx,item) {
            geojson.features.push({
                type     : "Feature",
                geometry : {
                    type        : "Point",
                    coordinates : [ $(item).data("properties").Lon, $(item).data("properties").Lat ]
                },
                properties : $(item).data("properties")
            });
        });
        return geojson;
    };
    
    // destroy
    //   removes the widget from the creation element and clears all resources associated with it
    //   the widget is no longer usable after it is destroyed
    o.destroy = function() {
        var funcName = "[destroy "+this.id+"]: ";
        // disconnect listeners
        $(window).off( "resize.search_api_"+this.id );
        this._getTextbox().add( this._getIcon() ).add( this._getMenu() ).off(".search_api");
        // remove menu
        this._getMenu().remove();
        // clear widget element
        $(this.id).empty();
        // clear widget obj
        delete search_api._widgets[this.id];
        var temp = this;
        $.each(temp,function(prop,val){
            delete temp[prop];
        });
        // done
        search_api._console(funcName+"widget destroyed and no longer usable");
        return this;
    };
    
    //................
    // private props
    // (none)
    
    //................
    // public props
    
    // geojson point feature object of last search result with location information set as properties
    o.result = undefined;
    
    //................
    // set input opts and return obj
    search_api._console("[create #"+id+"]: widget created");
    return o.setOpts(opts);
    
}; // create


//===========================================================
// PRIVATE METHODS (not documented)
//===========================================================

//-----------------------------------------------------------
// _setup
//   setup the widget
//
// input: 
//   widget object
//
// output:
//   widget object
search_api._setup = function(o) {
    
    //..............................
    // textbox and container
    
    // set some text box options:
    o._getTextbox()
        .attr( "placeholder", o.opts.placeholder )
        .prop( "title",       o.opts.tooltip     );
    
    // set size and width options
    o._getContainer()
        .removeClass("search-api-lg search-api-md search-api-sm search-api-xs")
        .addClass( "search-api-"+o.opts.size )
        .css({ width: (o.opts.width>0 ? o.opts.width+"px" : "") });
    
    //..............................
    // suggestion menu
    
    // clear cache
    o._menu.cache = {};
    
    // set size option
    o._getMenu()
        .removeClass("search-api-lg search-api-md search-api-sm search-api-xs")
        .addClass( "search-api-"+o.opts.size );
    
    // function to set menu position
    o._menu.position = function() {
        var Hwin     = $(window).height();
        var HtoTop   = o._getTextbox().offset().top - $(document).scrollTop();
        var HtoBot   = Hwin - HtoTop;
        var cssTop   = "initial";
        var cssBot   = "initial";
        var cssMH    = "initial";
        var addClass = "";
        if (HtoBot > HtoTop) {
            // more space below - open below
            cssTop   = o._getTextbox().offset().top + o._getTextbox().outerHeight();
            cssMH    = 0.9*HtoBot;
            addClass = "search-api-menu-down";
        } else {
            // more space above - open above
            cssBot   = Hwin - o._getTextbox().offset().top;
            cssMH    = 0.9*HtoTop;
            addClass = "search-api-menu-up";
        }
        o._getMenu()
            .css({
                left   : o._getTextbox().offset().left - o._getIcon().outerWidth(),
                width  : o._getIcon().outerWidth() + o._getTextbox().outerWidth() + o._getSpinner().outerWidth(),
                top    : cssTop,
                bottom : cssBot
            })
            .removeClass("search-api-menu-down search-api-menu-up")
            .addClass( addClass )
            .find(".search-api-menu-content")
                .css({
                    "max-height" : ( o.opts.menu_height>0 ? Math.min(o.opts.menu_height,cssMH) : cssMH ) // specified height or available space
                });
    };
    
    // re-position menu when window resizes
    $(window)
        .off( "resize.search_api_"+o.id )
        .on(  "resize.search_api_"+o.id, o._menu.position );
    
    // function to close menu
    o._closeMenu = function() {
        o._getMenu().stop().fadeOut(200, // close menu
            function() {
                o._getMenu().empty();    // clear contents
                o.trigger("update");     // trigger event
            }
        );
        return this;
    };
    
    // function to update menu with suggestions
    //   term - search term to use for item regex highlighting
    //   data - array of results, each element is a menu item
    o._menu.update = function( term, data ) {
        
        // see if no items
        if (data.length<=0) {
            o._getTextbox().addClass("search-api-no-suggestions"); // set text box class
            o._closeMenu();                                        // close menu
            return;
        }
        
        // have items
        o._getTextbox().addClass("search-api-have-suggestions");
        
        // clear menu and add wrapper content
        o._getMenu().html(
            // title bar
            '<div class="search-api-menu-title">' +
                (data.length>=o.opts.menu_max_entries ? 'Top ' : '') + data.length+' Results' +
                '<div class="search-api-menu-close" onclick="search_api._widgets[ $(this).closest(\'.search-api-menu\').data(\'id\') ]._closeMenu();">&#10006;</div>' +
            '</div>' +
            // content containing items
            '<div class="search-api-menu-content"></div>'
        );
        
        // regex for highlighting search term in item
        // regex on term with special regex characters can cause error
        var regex;
        try { 
            // all space-separated parts of term occurring in item name are highlighted
            regex = new RegExp("(" + term.replace(/\*/g,"").split(" ").join("|") + ")", "gi"); // need remove all "*" wild cards
        } catch(err) {
            // no highlighting
            regex = new RegExp("()");
        }
        
        // add items to content
        var curr_cat = "X";
        $.each( data, function(idx,item) {
            // add category separator if new category
            if (item.Category !== curr_cat) {
                $('<div class="search-api-menu-separator">' + item.Category + (item.PercentMatch!==100 ? " (Closest Matches)" : "") + '</div>').appendTo( o._getMenu().find(".search-api-menu-content") );
                curr_cat = item.Category;
            }
            // add item with name regex highlighted and data properties added
            $(
                '<div class="search-api-menu-item">' +
                    item.Name.replace( regex, '<span class="search-api-menu-item-highlight">$1</span>' ) + // 1st line: name
                    ( item.County!=="N/A"&&item.State!=="N/A" ? ' <br/><span class="search-api-menu-item-info">' + item.County + ", " + item.State + '</span>' : "" ) + // 2nd line: county, state
                    ( item.PercentMatch!==100                 ? ' <br/><span class="search-api-menu-item-info">' + item.PercentMatch  + '% match'              : "" ) + // 3rd line: percent match if not 100%
                '</div>'
            ).data("properties",item).appendTo( o._getMenu().find(".search-api-menu-content") );
        });
        
        // if there is only 1 item select it
        if (data.length==1) {
            o._getMenu().find(".search-api-menu-item").addClass("search-api-menu-item-selected");
        }
        
        // open menu
        o._menu.position();              // position
        o._getMenu().stop().fadeIn(200); // show
        o._menu.position();              // position again - scroll bar can affect left offset
        o.trigger("update");             // trigger event
    };
    
    //..............................
    // text box event listeners
    o._getTextbox()
        .off("blur.search_api")
        .on( "blur.search_api", function(){
            // close menu when text box loses focus
            o._closeMenu();
            
        })
        .off("keyup.search_api  input.search_api  propertychange.search_api  paste.search_api")
        .on( "keyup.search_api  input.search_api  propertychange.search_api  paste.search_api", function(e) {
            if ( $.inArray(e.which, [
                // do nothing if these keys pressed:
                13, // enter
                27, // escape
                33, // page up
                34, // page down
                35, // end
                36, // home
                37, // left arrow
                38, // up arrow
                39, // right arrow
                40  // down arrow
            ]) > -1 ) { return; }
            search_api._search(o); // perform search
        })
        .off("keydown.search_api")
        .on( "keydown.search_api", function(e){
            switch(e.which) {
                case 38: // arrow up
                case 40: // arrow down
                    
                    // advance selected item up or down
                    if (o._getMenu().html()==="") { return; } // no items
                    var sel = $(".search-api-menu-item-selected", o._getMenu()).eq(0);
                    var next;
                    if (sel.length<=0) {
                        // no selected item - goto 1st or last
                        next = (e.which === 40) ? $(".search-api-menu-item", o._getMenu()).first() : $(".search-api-menu-item", o._getMenu()).last();
                    } else {
                        // have selected item - goto next or previous
                        next = (e.which === 40) ? sel.nextAll(".search-api-menu-item").first() : sel.prevAll(".search-api-menu-item").first();
                    }
                    $(".search-api-menu-item-selected", o._getMenu()).removeClass("search-api-menu-item-selected");
                    next.addClass("search-api-menu-item-selected");
                    
                    // scroll menu to keep selected in view if needed
                    if (next.length>0) {
                        // go to next item
                        var maxHeight  = parseInt(o._getMenu().find(".search-api-menu-content").css("max-height"));
                        var itemHeight = next.outerHeight();
                        var scrTop     = o._getMenu().find(".search-api-menu-content").scrollTop();
                        var selTop     = next.offset().top - o._getMenu().find(".search-api-menu-content").offset().top;
                        if (selTop + itemHeight - maxHeight > 0) {
                            o._getMenu().find(".search-api-menu-content").scrollTop(selTop + itemHeight + scrTop - maxHeight);
                        } else if (selTop < 0) {
                            o._getMenu().find(".search-api-menu-content").scrollTop(selTop + scrTop);
                        }
                    } else {
                        // go to top
                        o._getMenu().find(".search-api-menu-content").scrollTop(0);
                    }
                    break;
                    
                case 27: // esc
                    
                    // close menu
                    o._closeMenu();
                    break;
                    
                case 13: // enter
                    
                    // click any selected item and close menu
                    var sel = $(".search-api-menu-item-selected", o._getMenu()).eq(0);
                    if (sel.length>0) { sel.click(); }
                    break;
            }
        });
    
    //..............................
    // menu event listeners
    o._getMenu()
        .off("mouseenter.search_api")
        .on( "mouseenter.search_api", ".search-api-menu-item", function() {
            
            // set selected item when mouse enters menu
            $(".search-api-menu-item-selected").removeClass("search-api-menu-item-selected");
            $(this).addClass("search-api-menu-item-selected");
            
        })
        .off("mouseleave.search_api")
        .on( "mouseleave.search_api", ".search-api-menu-item", function() {
            
            // remove selected item when mouse leaves menu
            $(".search-api-menu-item-selected").removeClass("search-api-menu-item-selected");
            
        })
        .off("click.search_api")
        .on( "click.search_api", ".search-api-menu-item", function(e) {
            
            // set result geojson
            o.result = {
                type    : "Feature",
                geometry: {
                    type       : "Point",
                    coordinates: [ $(this).data("properties").Lon, $(this).data("properties").Lat ]
                },
                properties: $(this).data("properties")
            };
            
            // close menu and clear text box
            o._closeMenu();
            o._getTextbox().val("");
            
            // trigger event
            o.trigger("result");
            
        });
    
    //..............................
    // save api widget for later access
    if (search_api._widgets===undefined) { search_api._widgets = {}; }
    search_api._widgets[o.id] = o;
    
    // return widget
    return o;
    
}; // _setup


//-----------------------------------------------------------
// _search
//   main search function
//   calls primary api service
//   if no results returned by api service calls secondary service
search_api._search = function(o) {
    var funcName = "[_search "+o.id+"]: ";
    
    // multiple textbox event triggers to handle all cases can call this repeatedly for same search text
    // do nothing if triggered multiple times for same search text
    if ( o._getTextbox().val() === o._menu.currentSearchVal ) { return; }
    o._menu.currentSearchVal = o._getTextbox().val();
    
    // cancel any previous timer and service calls still in progress
    clearTimeout(o._menu.timer);
    try { o._menu.xhr1.abort(); } catch(e){}
    try { o._menu.xhr2.abort(); } catch(e){}
    
    // clear text box classes
    o._getTextbox().removeClass("search-api-have-suggestions search-api-no-suggestions");
    
    // see if enough characters
    if ( $.trim( o._getTextbox().val() ).length < o.opts.menu_min_char ) {
        search_api._console(funcName+"not enough characters entered to search (at least "+o.opts.menu_min_char+" needed)");
        o._closeMenu();
        return;
    }
    
    // trigger on_search
    o.trigger("search");
    
    // get filtered search 'term' and search 'states' to use
    var term_states   = o._getSearchTermStates();
    var search_term   = term_states.term;
    var search_states = term_states.states;
    
    // see if lat-lon
    var lat_lon = search_term.split(/\s/);
    if (lat_lon.length<2) { lat_lon.push("X"); } // make at least 2 elements
    var ok  = true;
    var lat =           parseFloat(lat_lon[0]) ;  if (isNaN(lat) || lat < o.opts.lat_min || lat > o.opts.lat_max ) { ok = false; } // check
    var lon = -Math.abs(parseFloat(lat_lon[1]));  if (isNaN(lon) || lon < o.opts.lon_min || lon > o.opts.lon_max ) { ok = false; } // check - always make lon negative (western hemisphere)
    if (ok) {
        // valid - return
        search_api._console(funcName+"parsed as lat-lon");
        o._menu.update( search_term, [{
            Id           : null,
            Category     : "Latitude-Longitude Coordinate",
            Label        : "Coordinate "+lat+", "+lon,
            Name         : "Coordinate "+lat+", "+lon,
            County       : "N/A",
            State        : "N/A",
            ElevFt       : "N/A",
            GnisId       : "N/A",
            Lat          : lat,
            Lon          : lon,
            LatMin       : lat - 1e-6,
            LatMax       : lat + 1e-6,
            LonMin       : lon - 1e-6,
            LonMax       : lon + 1e-6,
            PercentMatch : 100,
            Source       : "LAT_LON"
        }] );
        return;
    }
    
    // see if in cache
    var term_cache = search_term + "|" + search_states;
    if (term_cache in o._menu.cache) {
        search_api._console(funcName+"cached value used");
        o._menu.update( search_term, o._menu.cache[term_cache] );
        return;
    }
    
    // // this will treat new characters added to a previous failure as a failure bypassing a new service request
    // // do not do this when using esri because adding new characters to a failed request can return results
    // // example: 'waashi' returns nothing, 'waashington' returns results
    // for (var n=term_cache.split("|")[0].length-1; n>o.opts.menu_min_char; n--) {
        // var term_substr = term_cache.substr(0,n) + "|" + search_states;
        // if (term_substr in o._menu.cache && o._menu.cache[term_substr].length<=0) {
            // search_api._console(funcName+"characters added to cached response that had no results");
            // o._menu.update( search_term, [] );
            // return;
        // }
    // }
    
    // new search after delay
    o._menu.timer = setTimeout( function() {
        
        // show spinner
        o._getSpinner().removeClass("search-api-spinner-hidden");
        
        // primary source: api service request
        search_api._console(funcName+"calling api data service: '"+search_term+"'");
        var results = [];
        if ($.support) { $.support.cors = true; } // need for IE
        o._menu.xhr1 = $.ajax({
            url  : search_api._apiServiceUrl,
            data : {
                term               : search_term,
                search_states      : search_states,
                lat_min            : o.opts.lat_min,
                lat_max            : o.opts.lat_max,
                lon_min            : o.opts.lon_min,
                lon_max            : o.opts.lon_max,
                include_gnis_major : o.opts.include_gnis_major,
                include_gnis_minor : o.opts.include_gnis_minor,
                include_state      : o.opts.include_state,
                include_zip_code   : o.opts.include_zip_code,
                include_area_code  : o.opts.include_area_code,
                include_usgs_sw    : o.opts.include_usgs_sw,
                include_usgs_gw    : o.opts.include_usgs_gw,
                include_usgs_sp    : o.opts.include_usgs_sp,
                include_usgs_at    : o.opts.include_usgs_at,
                include_usgs_ot    : o.opts.include_usgs_ot,
                include_huc2       : o.opts.include_huc2,
                include_huc4       : o.opts.include_huc4,
                include_huc6       : o.opts.include_huc6,
                include_huc8       : o.opts.include_huc8,
                include_huc10      : o.opts.include_huc10,
                include_huc12      : o.opts.include_huc12,
                top_n              : o.opts.menu_max_entries,
                caller             : search_api._caller
            },
            type     : "GET",
            dataType : "json",
            async    : true,
            cache    : true,
            timeout  : 3000, // adjust as needed
            success: function(json){
                // api service success
                // ...check
                if (json.error       !== undefined) { search_api._console(funcName+"api service error: "+json.error,         "warn"); return; }
                if (json.constructor !== Array    ) { search_api._console(funcName+"invalid api service response: not array","warn"); return; }
                // ...ok - set results
                results = json;
            },
            error: function(xhr,x,xx) {
                // api service error
                // ...log error if call wasn't aborted on purpose
                if ( xhr.statusText.toLowerCase() !== "abort" ) {
                    search_api._console(funcName+"api service error: "+xhr.statusText,"warn");
                }
            },
            complete: function() {
                // api service complete
                
                // see if have api results
                if (results.length > 0) {
                    search_api._console(funcName+"api service returned "+results.length+" results");
                    o._getSpinner().addClass("search-api-spinner-hidden"); // hide spinner
                    o._menu.update( search_term, results );                // update menu with results
                    o._menu.cache[term_cache] = results;                   // save to cache
                    return;                                                // done
                }
                
                // no api results - secondary service request
                // note on esri service: addresses not returned if Regions (states) specified
                search_api._console(funcName+"api service returned no results - calling secondary service");
                o._menu.xhr2 = $.ajax({
                    url  : search_api._2ndServiceUrl,
                    data : {
                        f            : "json", // required
                        singleLine   : search_term,
                        CountryCode  : "USA",
                        searchExtent : [o.opts.lon_min, o.opts.lat_min, o.opts.lon_max, o.opts.lat_max].join(","),
                        maxLocations : o.opts.menu_max_entries,
                        outFields    : [
                            "Type",      // Category
                            "Region",    // State (long name)
                            "Subregion", // County
                            "X",         // Lon
                            "Y",         // Lat
                            "Loc_name"   // Source
                        ].join(",")
                    },
                    type     : "GET",
                    dataType : "json",
                    async    : true,
                    cache    : true,
                    timeout  : 5*1000, // adjust as needed
                    success : function (json) {
                        // secondary service success
                        
                        // check
                        if (json.candidates === undefined) { search_api._console(funcName+"invalid secondary service response: no candidate field","warn"); return; }
                        search_api._console(funcName+"secondary service returned "+json.candidates.length+" total results");
                        if (json.candidates.length<=0) { return; }
                        
                        // have results - filter
                        var okStates  = search_states.split(",");  if (okStates[0]==="ALL"){ okStates = search_api._stateAbbrevs; }
                        var resultObj = {};
                        var addIds    = [];
                        $.each(json.candidates, function(idx,result) {
                            // check:
                            if (
                                result.address         === undefined ||
                                result.score           === undefined || isNaN(parseFloat(result.score))      || result.score < 95                    || // percent match threshold - adjust as needed
                                result.attributes      === undefined ||
                                result.attributes.Type === undefined || $.inArray( result.attributes.Type.toUpperCase(), search_api._2ndOkTypes )<0  ||
                                result.attributes.X    === undefined || result.attributes.X < o.opts.lon_min || result.attributes.X > o.opts.lon_max ||
                                result.attributes.Y    === undefined || result.attributes.Y < o.opts.lat_min || result.attributes.Y > o.opts.lat_max
                            ) {
                                return true; // skip
                            }
                            
                            // clean up attributes
                            $.each( result.attributes, function(name,val) {
                                val = $.trim(val);
                                result.attributes[name] = ( val!=="" ? val : "N/A" );
                            });
                            
                            // get state abbreviation, reject if none or not in allowed list
                            var State = search_api._statesLong2short[ result.attributes.Region.toUpperCase() ];
                            if (State===undefined || $.inArray(State,okStates)<0) { return true; }
                            
                            // get county, reject if none and add County at end
                            var County = result.attributes.Subregion;
                            if (County==="N/A") { return true; }
                            if (!/ County/i.test(County)) { County += " County"; }
                            
                            // set category and name
                            if (result.attributes.Type!=="N/A") {
                                // have type - use for category
                                var Category = result.attributes.Type;
                                var Name     = $.trim( result.address.split(",")[0] ); // remove county & state
                            } else {
                                // no type - address or other
                                var Category = "Addresses and Other Suggestions";
                                var Name     = $.trim( result.address );
                            }
                            
                            // add to resultObj for sorting
                            // same result can be returned from different sources - do not add duplicates
                            if (resultObj[Category]               ===undefined) { resultObj[Category]                = {}; }
                            if (resultObj[Category][State]        ===undefined) { resultObj[Category][State]         = {}; }
                            if (resultObj[Category][State][County]===undefined) { resultObj[Category][State][County] = []; }
                            var addId = [Category,State,County,Name].join("|");
                            if ( $.inArray(addId,addIds)<0 ) { // not a duplicate
                                addIds.push(addId);
                                resultObj[Category][State][County].push({
                                    Id           : null,
                                    Category     : Category,
                                    Label        : Name + " (" + County + ", " + State + ")",
                                    Name         : Name,
                                    County       : County,
                                    State        : State,
                                    ElevFt       : "N/A",
                                    GnisId       : "N/A",
                                    Lat          : parseFloat(result.attributes.Y),
                                    Lon          : parseFloat(result.attributes.X),
                                    LatMin       : result.extent.ymin,
                                    LatMax       : result.extent.ymax,
                                    LonMin       : result.extent.xmin,
                                    LonMax       : result.extent.xmax,
                                    PercentMatch : result.score,
                                    Source       : "ESRI:"+result.attributes.Loc_name
                                });
                            }
                        });
                        
                        // add to results sorted by category then state then county
                        var S1 = [];  $.each( resultObj, function(n){ S1.push(n); });
                        $.each( S1.sort(), function(idx,s1) {
                            var S2 = [];  $.each( resultObj[s1], function(n){ S2.push(n); });
                            $.each( S2.sort(), function(idx,s2) {
                                var S3 = [];  $.each( resultObj[s1][s2], function(n){ S3.push(n); });
                                $.each( S3.sort(), function(idx,s3) {
                                    results = results.concat( resultObj[s1][s2][s3] );
                                });
                            });
                        });
                    },
                    error: function(xhr) {
                        // secondary service error
                        // ...log error if call wasn't aborted on purpose
                        if ( xhr.statusText.toLowerCase() !== "abort" ) {
                            search_api._console(funcName+"secondary service error: "+xhr.statusText,"warn");
                        }
                    },
                    complete: function(xhr) {
                        // secondary service complete
                        search_api._console(funcName+"secondary service returned "+results.length+" results after filtering");
                        o._getSpinner().addClass("search-api-spinner-hidden"); // hide spinner
                        o._menu.update( search_term, results );                // update menu with results
                        if ( xhr.statusText.toLowerCase() !== "abort" ) {      // save to cache if not aborted
                            o._menu.cache[term_cache] = results;
                        }
                        if (results.length<=0) { o.trigger("failure"); }       // trigger failure if no results
                    }
                    
                }); // secondary ajax
                
            } // api ajax complete
            
        }); // api ajax
        
    }, 50 ); // adjust delay between service calls as needed
    
}; // _search


//-----------------------------------------------------------
// _console
//   log a debug, warning, or error message to console
//   if the "search_api.verbose" option is set false debug logging is suppressed
//   the verbose option does not suppress warnings or errors
//
// input:
//    msg  - message string to log
//    mode - one of:
//             "debug" - log debug message (default)
//             "warn"  - log warning
//             "error" - log error
// output:
//   (none)
search_api._console = function( msg, mode ) {
    if (!window.console) { return; }
    if (msg  === undefined) { msg  = ""   }   // default msg
    if (mode === undefined) { mode = "D"; }   // default mode
    mode = mode.substring(0,1).toUpperCase(); // just look at 1st letter
    if (mode === "D" && !search_api.verbose) { return 0; } // no debug log if verbose not true
    switch(mode) { // log msg using mode
        case "W": console.warn ( (new Date()).toLocaleTimeString() + " search_api " + search_api.version + " WARNING " + msg ); break;
        case "E": console.error( (new Date()).toLocaleTimeString() + " search_api " + search_api.version + " ERROR "   + msg ); break;
        case "D": console.log  ( (new Date()).toLocaleTimeString() + " search_api " + search_api.version + " "         + msg ); break;
        default : console.log  ( (new Date()).toLocaleTimeString() + " search_api " + search_api.version + " "         + msg ); break;
    }
}; // _console


// load msg
if (search_api._isLoadedJq) {
    console.log( (new Date()).toLocaleTimeString() + " search_api " + search_api.version + " [load]: api loaded and available" );
}

//===========================================================
// leaflet plugin - available when leaflet loaded before api
//===========================================================
if (
    window.L           !== undefined &&
    L.Control          !== undefined &&
    L.Control.extend   !== undefined &&
    L.control          !== undefined &&
    L.Map              !== undefined &&
    L.Map.mergeOptions !== undefined &&
    L.Map.addInitHook  !== undefined 
) {
    
    // constructor
    L.Control.Search = L.Control.extend({
        
        //------------------------------
        // public (documented)
        //------------------------------
        
        //.............................
        // creation options
        options : {
            
            // standard leaflet control options
            position: "topright",
            
            // whether to automatically connect basic on_result handler to zoom map to result and show popup when on_result is not input
            autoResult: true,
            
            // all search_api.create() options supported as additional properties
        },
        
        //.............................
        // methods
        
        // get search widget object for accessing widget properties and methods after control creation
        getWidget: function() {
            return this._widget;
        },
        
        //------------------------------
        // private (undocumented)
        //------------------------------
        
        onAdd: function(map) {
            
            // add control div for widget with a unique incremented id
            var div = L.DomUtil.create( "div", "", map.getContainer() );
            var n = 1;  while( L.DomUtil.get("search_api_"+n) !== null ) { n++; }
            div.id = "search_api_"+n;
            
            // disable click propagation so clicking widget does not fire map click
            L.DomEvent.disableClickPropagation(div);
            
            // make copy of control input options for search_api.create()
            var opts = L.Util.extend({},this.options);
            
            // add basic on_result callback if autoResult and on_result not input
            if (opts.autoResult===true && typeof opts.on_result!=="function") {
                opts.on_result = function(o) {
                    map
                        .fitBounds([ // zoom to location
                            [ o.result.properties.LatMin, o.result.properties.LonMin ],
                            [ o.result.properties.LatMax, o.result.properties.LonMax ]
                        ])
                        .openPopup( // open popup with label and category
                            '<div style="text-align:center;">'+
                                '<b>'+ o.result.properties.Label +'</b><br/>'+
                                o.result.properties.Category +
                            '</div>',
                            [ o.result.properties.Lat, o.result.properties.Lon ]
                        );
                }
            }
            
            // remove non-api options
            delete opts.position;
            delete opts.autoResult;
            
            // create widget using opts and return control div
            this._widget = search_api.create( div.id, opts );
            return div;
        },
        
        onRemove: function(map) {
            // destroy widget
            this._widget.destroy();
        }
        
    }); // end constructor
    
    // constructor registration
    L.control.search = function(options) {
        return new L.Control.Search(options);
    };
    
    // add new map constructor option to create search control
    L.Map.mergeOptions({
        searchControl: false
    });
    
    // add search control when map created if option set
    L.Map.addInitHook( function() {
        if (typeof this.options.searchControl === "object") {
            this.searchControl = ( new L.Control.Search( this.options.searchControl ) ).addTo(this);
        }
    });
    
    // done
    console.log( (new Date()).toLocaleTimeString() + " search_api " + search_api.version + " [load]: Leaflet plugin loaded and available" );
}

//===========================================================
// END
//===========================================================

var whoLogin = {

    // Run an ajax query and update the ajax status bar
    get: function(req) {

        whoLogin.update_ajax_status("Loading ...", "/img/loading.gif");

        // Set success and error handlers
        var req_success = req.success;
        req.success = function (data, textStatus, xmlHttpRequest) {
            whoLogin.update_ajax_status("Loading ... Done.", "/img/ok.png");
            if (typeof(req_success) == "function")
                req_success(data, textStatus, xmlHttpRequest);
        };
        req.error = function (xmlHttpRequest, textStatus, errorThrown) {
            whoLogin.update_ajax_status("Loading ... Error while contacting the server.", "/img/error.png");
        };

        // Some defaults
        req.dataType = "json";

        $.ajax(req);

    },

    update_ajax_status: function (msg, img) {
        $("div#ajax-status p"  ).text(msg);
        $("div#ajax-status img").attr("src", img);
    },

    // Set the currently active website in the menu
    set_menu: function (sel) {
        $(".menu-item a").not(sel).removeClass("menu-active");
        $(sel).addClass("menu-active");
    },

    //
    // Get & display pages
    //

    init: function () {
        whoLogin.get({
            url:        "/init",
            success:    function (data) {
                            // Add events to menu
                            $("#menu-status"  ).click( whoLogin.stat.get );
                            $("#menu-settings").click( whoLogin.settings.get ); 

                            // Show status/settings
                            switch (data.type) {
                                case "status": whoLogin.stat.init(data);
                                               break;
                                default:       whoLogin.settings.init(data);
                                               break;
                            };
                        }
        });
    },


    // 
    // Status
    //

    stat: {

        get: function () {
            whoLogin.get({
                url:        "/status",
                success:    whoLogin.stat.init
            });
        },

        init: function (data) {

            whoLogin.set_menu("#menu-status");
            $("section#main").html(data.html);

            // Set the new connection color
            var color = data.connected ? "#2d2" : "#d22";
            $("#status-cur").css("color", color);

        },

    },


    //
    // Settings
    //

    settings: {

        get: function () {
            whoLogin.get({
                url:        "/settings",
                success:    whoLogin.settings.init
            });
        },

        init: function (data) {
            whoLogin.set_menu("#menu-settings");
            $("section#main").html(data.html);

            // Load "Advanced settings" data on click
            $("#settings-advanced p a").click( whoLogin.settings.get_advanced );

            // Handle form submit
            $("#settings-form").submit( whoLogin.settings.submit );
        },

        // Get the "Advanced settings" data
        get_advanced: function () {
            whoLogin.get({
                url:        "/settings/advanced",
                success:    function (data) {
                                $("#settings-advanced-content").html(data.html);
                                $("#settings-advanced-content").show();
                                $("#settings-advanced p a").unbind("click");
                            },
            });
        },

        // Submit the settings form
        submit: function (data) {
            whoLogin.get({
                url:        "/settings/post",
                type:       "POST",
                data:       $("#settings-form").serialize(),
                success:    whoLogin.settings.show_result,
            });
            return false; // prevent form from submitting
        },

        show_result: function (data) {
            if (data.success) {
                $(".form-error").hide().text("");
                $("#settings-updated") .show();
            } else {
                $("#settings-updated").hide();
                for (id in data.errors) {
                    $("#" + id).text(data.errors[id])
                               .css("display", "inline-block");
                }
            }

        },
    },
}


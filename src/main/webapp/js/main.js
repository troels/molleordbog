(function ($, window) {
     var molle = window.molle = window.molle || {};

     molle.showSources = function (baseElem) {
         $(baseElem).parents(".word").find(".source").show();
         $(baseElem).remove();
     };
})(jQuery, window);
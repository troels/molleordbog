/*jslint strict: true, browser: true */
/*globals alert: true */

(function ($, window) {
     "use strict";


     var molle = window.molle = window.molle || {};
     var enable_assert = true;

     function assert(cond, msg) {
         if (enable_assert && window.console && window.console.trace) {
             if (!cond) {
                 window.console.trace(msg || "Expected cond to be true, was false");
             }
         }
     }

     function assertEquals(a, b) {
         assert(a === b, a + " was not equal to " + b);
     }


     function jqesc(str) {
         return str.replace(/[#;&,.+*~':"!\^$\[\]()=>|\/]/g, function (str) {
                                return "\\" + str;
                            });
     }

     molle.showSources = function (baseElem) {
         $(baseElem).parents(".word").find(".source").show();
         $(baseElem).remove();
     };

     molle.gotoPage = function(page) {
         window.location.href = page;
     };


     function ExcisionHolder(id) {
         this.img_sel = "#" + jqesc(id);

         var img = $(this.img_sel);

         this.dims = img.offset();
         this.dims.width = img.width();
         this.dims.height = img.height();

         var newDiv = $("<div />").css({
                                       "position": "absolute",
                                       "left": this.dims.left,
                                       "top": this.dims.top,
                                       "width": this.dims.width,
                                       "height": this.dims.height,
                                       "z-index": 10
                                   });
         this.div = newDiv;
         $("body").append(newDiv);
         this.excisions = [];
     }

     var excisionZIndex = 1000;
     ExcisionHolder.prototype = {
         addExcision: function (x, y, width, height, destination, name, title) {
             function opacitySetter(opacity) {
                 return function (e) {
                     $(this).children("div").fadeTo(0, opacity);
                     if (e) {
                         e.stopPropagation();
                         e.preventDefault();
                     }
                 };
             }

             var lightOpacity = 0.3, darkOpacity = 0.8,
                 outerDiv =
                 $("<div />")
                   .css({
                            position: "absolute",
                            left: x,
                            top: y,
                            width: width,
                            height: height,
                            border: "1px solid",
                            cursor: "pointer",
                            "z-index": excisionZIndex--
                        })
                    .html($("<div/>")
                          .attr("title", title)
                          .css({
                                   "width": "100%",
                                   "height": "100%",
                                   "background-color": "#74A6D7",
                                   "opacity": lightOpacity
                               }))
                 .mouseover(opacitySetter(darkOpacity))
                 .hover(opacitySetter(darkOpacity), opacitySetter(lightOpacity))
                 .click(function (e) {
                            if (destination) {
                                window.location.href = destination;
                                opacitySetter(lightOpacity).apply(this);
                            } else if (name) {
                                molle.subjectAccordion.accordion(
                                    "activate", "#subjectcontainer h3:contains(" + jqesc(name) + ")");
                            }
                        });

             this.div.append(outerDiv);
         }

     };
     molle.makeExcisionHolder = function(id) { return new ExcisionHolder(id); };

     molle.activateSynonym = function (synonym, synonyms) {
         $(".word.marked").toggleClass("marked");
         $(".word").filter(
             function () {
                 return !!(new RegExp("^\\s*" + synonym + "[,\\s]*$").exec($(this).text()));
             })
             .toggleClass('marked');

         synonyms.pop();
         if (synonyms.length == 0) {
             $("#synonymbox").html();
             return;
         }
         var dom = $("<div />").css("margin-top", "10px");

         for (var i = 0; i < synonyms.length; ++i) {
             dom.append($("<a />").attr('href', '/kilder/viskilde/' + synonyms[i] + '/').text(synonyms[i]));
             if (i != synonyms.length - 1) {
                 dom.append(", ");
             }
         }

         var outerDiv = $("<div />")
             .css("margin-top", "10px")
             .append("Bliver brugt i egnen" + (synonyms.length > 1 ? "e" : "" ) + ":")
             .append(dom);

         $("#synonymbox").html(outerDiv);
     };

     $(function () {
	         $('#nav li').hover(
		           function () {
			             $('ul', this).slideDown(100);
                   $(this).addClass('nav-marked');
		           },
		           function () {
			             $('ul', this).slideUp(100);
                   $(this).removeClass('nav-marked');
		           }
	         );
       });
}(jQuery, window));


var loc = window.location.href,
    index = loc.indexOf('#');

if (index > 0) {
  window.location = loc.substring(0, index);
}

$.fn.exists = function () {
    return this.length > 0 ? this : false;
}
	//Handle window resize
	var resizeFlag=0;  
	$(window).resize(function(){
	    resizeFlag=1;
	})
	checkSizeChange();
	function checkSizeChange(){
	    if (resizeFlag) {
	      resizeFlag=0;
	      $(window).trigger('resized');
	    }
	    setTimeout(function(){
	      checkSizeChange();
	    }, 200);
	}

$(document).ready(function(){

	/*++++++++++++++++++++++++++++++++++++
		tooltips
	++++++++++++++++++++++++++++++++++++++*/
	$(".tooltips").tooltip();


	/*++++++++++++++++++++++++++++++++++++
		sidebar
	++++++++++++++++++++++++++++++++++++++*/
	var sideS, sidebar = {
		settings : {
			$side : $("#sidebar"),
			$main : $("#main"),
			$trigger : $("a.mobilemenu"),
			$totaltrigger : $(".social-icons, #main-nav, #main"),
			$sideFooter:$('#sidebar-footer'),
			$sideContent:$('#main-nav'),
			$sidebarWrapper:$('#sidebar-wrapper'),
			windowWidth:$(window).width()
			
		},

		init : function(){
			sideS = this.settings;
			sideS.contentPadding=parseInt(sideS.$sideContent.css('padding-bottom'));
			this.customScrollFlag=false;
			this.setScrollBar();
			this.setContentPadding();
			this.bindUiActions();
			this.setMobileSide();
		},

		isIn : function(){
			return (sideS.$main.hasClass("sideIn"));
		},

		bindUiActions : function(){
			var self = this;
			sideS.$trigger.click(function(e){
				e.preventDefault();
				if (self.isIn()){
					self.sideOut();
				}else{
					self.sideIn();
				}
			});
			sideS.$totaltrigger.click(function(){
				if ($(window).width() < 960 && self.isIn())
					self.sideOut(); 
			});

			sideS.$side.on('afterSlideIn',function(){
				if($(window).width()<=600){// under 600px sidebar will cover the whole screen
					sideS.$main.css('display','none');
				}
			});

			sideS.$side.on('beforeSlideOut',function(){
				if($(window).width()<=600){
					sideS.$main.css('display','block');
				}
			});

			$(window).on('resized',function(){
				sideS.windowWidth=$(window).width();
				self.setContentPadding();
				self.setScrollBar();
				if ( sideS.windowWidth > 991 ){
					self.reset();
				}else{
					self.gomobile();
				}

				self.setMobileSide();
			});
		},

		sideIn : function() {
			var self = this;
			var sWidth=sideS.$side.width();
			sideS.$side.trigger('beforeSlideIn');
			var SidebarAnimIn = new TimelineLite({onComplete:function(){
				sideS.$side.trigger('afterSlideIn');
			}});
			SidebarAnimIn.to(sideS.$main,0.5,{left:sWidth,right:-sWidth,ease:Power2.easeIn},"-=0.2");
			sideS.$main.addClass('sideIn');
		},

		sideOut : function(){
			var self = this;
			sideS.$side.trigger('beforeSlideOut');
			var SidebarAnimOut = new TimelineLite({onComplete:function(){
				sideS.$side.trigger('afterSlideOut');
			}});
			SidebarAnimOut
				//.to(sideS.$side,0.2,{left:-250})
				.to(sideS.$main,0.5,{left:0,right:0,ease:Power2.easeIn},"-=0.2");
			sideS.$main.removeClass('sideIn');
		},

		reset : function(){
			sideS.$main.css({left:250, right:0});
			sideS.$side.css({left:0});
			sideS.$main.addClass('sideIn');
		},

		gomobile : function (){
			sideS.$main.css({left:0, right:0});
			sideS.$side.css({left:0});	
			sideS.$main.removeClass('sideIn');
		},
		setMobileSide:function(){
			var self=this;

			var tWidth=$(window).width();
			
			if (tWidth<600){
				sideS.$side.width(tWidth);
			}else{
				sideS.$side.width('');
				sideS.$main.css('display','block');
			}
			
		},
		setContentPadding:function(){
			var self=this;
			var footerHeight=sideS.$sideFooter.outerHeight();
			sideS.$sideContent.css({paddingBottom:sideS.contentPadding+footerHeight});
			
		},
		setScrollBar:function(){
			var self=this;

			if ((sideS.windowWidth>1024 || !isTouchSupported()) && !self.customScrollFlag){//Considiton that we want custom scrollbar
				self.setCustomScroll();
			}else if(sideS.windowWidth<=1024 && isTouchSupported() && self.customScrollFlag){ // Condition that we don't want custom scrollbar
				self.destroyCustomScroll();
			}
			
		},
		setCustomScroll:function(){

			this.customScrollFlag=true;
			sideS.$sidebarWrapper.niceScroll({
				horizrailenabled:false,
				cursorwidth: '6px',
			    cursorborder: 'none',
			    cursorborderradius:'0px',
			    cursorcolor:"#aaa"
			});
		
		},destroyCustomScroll:function(){
			sideS.$sidebarWrapper.niceScroll('destroy');
			this.customScrollFlag=false;

		}

	}
	sidebar.init();



	/*++++++++++++++++++++++++++++++++++++
		pages 
	++++++++++++++++++++++++++++++++++++++*/
	var pager = {
		pageContainer : $("div#main"),
		pages : $("div.page"),
		menuItems: $("ul#navigation"),
		overlay : $("div#overlay"),
		topz : "500",
		init: function(){

			self = this;
			self.menuItems.on('click','li:not(.external)', function(e){
				
				e.preventDefault();
				var $li = $(this),
					$target = $($li.children('a').attr('href')),
					currentPosition = $target.attr('data-pos'),
					$secondary = self.pageContainer.children(".currentpage");
				switch (currentPosition){
					case "home" :
						self.reset();
						break;
					case "p1" :
						self.forward($target,$secondary);
						break;
					case "p3" :
						if ( parseInt($target.attr('data-order')) === self.maxz() )
						{	
							// var $gotop2 = $target,
							// 	$gotop1 = $secondary;
							self.backward($target,$secondary);
						} else {
							self.forward($target,$secondary);
						}
					break;
					default:
						return false;
				}
			});

			self.overlay.on('click',function(){
				var $secondary = self.pageContainer.children(".currentpage");
				var $target = self.pageContainer.children("[data-order="+self.maxz()+"]");
				self.backward($target,$secondary);
			});

		},

		reset : function (){

			this.overlay.hide();
			
			var $gotop1 = this.pages.not(".home");
			$gotop1.attr('data-pos','p1').removeAttr('data-order');
			TweenLite.to($gotop1,0.4,{left:"100%",zIndex:0, onComplete:function(){
				$gotop1.removeClass('currentpage');	
			}});

			this.hanndelMenu();
		},

		forward : function(gotop2 , /* optional */ gotop3){
			
			

			self.hanndelMenu(gotop2);
			self.overlay.show();
			var maxz = self.maxz();
			gotop2.addClass('currentpage');
			gotop2.attr('data-pos','p2').removeAttr('data-order');
			gotop3.attr('data-pos','p3').attr('data-order',maxz+1);
			

			( new TimelineLite() )
				.set(gotop2,{ left:"100%",zIndex:self.topz})
				.set(gotop3,{zIndex:maxz+1})
				.to(gotop2,0.4,{left:"15%"})
				.to(gotop3,0.3,{ left:0 , onComplete:function(){gotop3.removeClass('currentpage');} },"-=0.2");
		},

		backward : function (gotop2,gotop1){
			

			this.hanndelMenu(gotop2);
			gotop2.exists() || this.overlay.hide();
			gotop2.addClass('currentpage').removeAttr('data-order').attr('data-pos',"p2");
			gotop1.attr('data-pos','p1');

			(new TimelineLite())
				.set(gotop2,{zIndex:self.topz-1})
				.to(gotop2,0.4,{left:"15%"})
				.to(gotop1,0.5,
					{
						left:"100%",
						onComplete : function(){
							gotop1.removeClass('currentpage');
						}
					},"-=0.3")
				.set(gotop1,{zIndex:0});

		},

		maxz : function(){
			
			var levelArray = this.pages.map( function() {
			    return $(this).attr('data-order');
			}).get();
			maxz = levelArray.length && Math.max.apply( Math, levelArray );
			return maxz;
		},

		hanndelMenu : function(){
			

			var menuIndex = ( arguments.length ) ? ( (arguments[0].length) ? arguments[0].index() : 0 ):0;

			this.menuItems.children().eq(menuIndex)
				.addClass('currentmenu')
				.siblings().removeClass('currentmenu');
			

		}
	}



	pager.reset();
	pager.init();


	/*++++++++++++++++++++++++++++++++++++
		click event on ul.timeline titles
	++++++++++++++++++++++++++++++++++++++*/
	$("ul.timeline").children().eq(0)
		.find(".text").slideDown()
		.addClass("open");

	$("ul.timeline").on("click","li", function(){
		$this = $(this);
		$this.find(".text").slideDown();
		$this.addClass("open");
		$this.siblings('li.open').find(".text").slideUp();
		$this.siblings('li').removeClass("open");
	}).on('mouseenter','li',function(){
		$this = $(this);
		var anim = new TweenLite($(this).find(".subject"),0.4,{'padding-left':20, paused:true});
		($this.hasClass('open')) || anim.play();
	}).on('mouseleave','li',function(){
		var anim = new TweenLite($(this).find(".subject"),0.2,{'padding-left':0});
	});



	/*++++++++++++++++++++++++++++++++++++
		lab personnel carousel
	++++++++++++++++++++++++++++++++++++++*/
	function generateLabCarousel(suffix) {

		var defaultCss = {
			width: 100,
			height: 100,
			marginTop: 50,
			marginRight: 0,
			marginLeft: 0,
			opacity: 0.2
		};
		var selectedCss = {
			width: 150,
			height: 150,
			marginTop: 30,
			marginRight: -25,
			marginLeft: -25,
			opacity: 1
		};
		var aniOpts = {
			queue: false,
			duration: 300,
			//easing: 'cubic'
		};
		var $car = $('#lab-carousel' + suffix);
		$car.find('img').css('zIndex', 1).css( defaultCss );

		$car.children('div').each(function(i){
			$(this).data('index',i);
		});

		$car.carouFredSel({
			circular: true,
			infinite: true,
			width: '100%',
			height: 205,
			items: {
				visible: 3,
			    minimum:3,
			    start:"div #first"
			},
                        cookie: false,
			prev: function() { return $(this).parent().parent().find('#prev' + suffix) },
			next: function() { return $(this).parent().parent().find('#next' + suffix) },
			auto: false,
			swipe : {
				onTouch :true,
				onMouse :true
			},
			scroll: {
				items: 1,
				duration: 300,
				//easing: 'cubic',
				onBefore: function( data ) {
					var $comming = data.items.visible.eq(1),
						$going = data.items.old.eq(1),
						$commingdetail = $("div#lab-details" + suffix + " div").eq($comming.data('index')),
						$goingdetail = $("div#lab-details" + suffix + " div").eq($going.data('index'));

					
					$goingdetail.fadeOut(100,function(){
						$goingdetail.siblings().hide();
						$commingdetail.fadeIn(300);
					});
					

					$comming.find('img').css('zIndex', 2).animate( selectedCss, aniOpts );
					data.items.old.eq(1).find('img').css('zIndex', 1).animate( defaultCss, aniOpts );
				}
			}

		});
	}
	generateLabCarousel('-1');
        generateLabCarousel('-2');
        generateLabCarousel('-3');

	/*++++++++++++++++++++++++++++++++++++
		Publications page categorization
	++++++++++++++++++++++++++++++++++++++*/
	
	function InitializePublications() {
	    $('div#pub-grid').mixitup({
		layoutMode: 'list',
		easing : 'snap',
		transitionSpeed :600,
		onMixEnd: function(){
		    $(".tooltips").tooltip();
		}
	    }).on('click','div.pubmain',function(){
		var $this = $(this), 
		$item = $this.closest(".item");
		
		$item.find('div.pubdetails').slideToggle(function(){
		    $this.children("i").toggleClass('icon-collapse-alt icon-expand-alt');
		},function(){
		    $this.children("i").toggleClass('icon-expand-alt icon-collapse-alt');
		});
	    });
	    
	    $( '#cd-dropdown' ).dropdownit( {
		gutter : 0
	    } );
	    
	    $("[name=cd-dropdown]").on("change",function(){
		var item = this.value;		
		$('div#pub-grid').mixitup('filter',item);
	    });

	    //$('div#pub-grid').mixitup('sort', ['data-yearMonth', 'asc']);

	}

    function encodeTopicAndLabel(topicCode) {
	var topic = encodeTopic(topicCode);
	var label = encodeTopicLabel(topicCode);
	return { "topic": topic, "label": label };
    }
	
    function encodeTopic(code) {
	switch(code) {
	case 'csed': return 'CS Education';
	case 'biometrics': return 'Biometrics';
	case 'hase': return 'Human Aspects';
	case 'workshop': return 'Workshop';
	case 'talk': return 'Invited Talk';
	case 'dissertations': return 'Dissertation';
	case 'proglang': return 'Programming Languages';
	case 'tool': return 'Tool';
	case 'agile': return 'Agile Methods';
	    case 'gsd': return 'Global Software Development';
	case 'socialmedia': return 'Social Media';
case 'poster': return 'Poster';
	case 'panel': return 'Panel';
	case 'accessibility': return 'Accessibility';
	default: return 'Other';
	}
    }

    function encodeTopicLabel(code) {
	switch(code) {
	case 'csed': return 'label-primary';
	case 'biometrics': return 'label-success';
	case 'hase': return 'label-warning';
	case 'workshop': return 'label-danger';
	case 'talk': return 'label-info';
	case 'dissertations': return 'label-default';
	case 'proglang': return 'label-primary';
	case 'tool': return 'label-danger';
	case 'agile': return 'label-info';
        case 'gsd': return 'label-danger';
	case 'socialmedia': return 'label-info';
case 'poster': return 'label-danger';
	case 'panel': return 'label-danger';
	case 'accessibility': return 'label-primary';
	default: return 'label-default';
	}
    }

    function urlify(text) {
	var result = URI.withinString(text, function(url) {
	    return "<a href=\"" + url + "\">" + url + "</a>";
	});
	return result;

    }


    function getLastModifiedDate(url) {
	try {
            var req=new XMLHttpRequest();
            req.open("GET", url, true);
	    req.send();
            if(req.status== 200){
		return new Date(req.getResponseHeader('Last-Modified'));
            }
            else return false;
	} catch(er) {
            return er.message;
	}
    }

    function comparePubs(a, b) {
	if (a.Fields.yearMonth < b.Fields.yearMonth) {
	    return 1;
	} else if (a.Fields.yearMonth > b.Fields.yearMonth) {
	    return -1;
	} else {
	    return 0;
	}
    }

    var pubtemplate = $('#pubtemplate').text();
    var templateFn = doT.template(pubtemplate);

    function modifyDOMforPubs(processedJSON, templateFn) {
	var node = $("div.pitems");
	var children = node.children();
	var childrenIds = $.map(children, function (elt) { 
	    return elt.id; 
	});
	var allText = [];
	$.each(processedJSON.entries, function(key, val) {
	    try {
		var updatedKey = val.EntryKey.replace(/[:]/g,"\\:").replace(/[.]/g,"\\.");
		if ($.inArray(updatedKey, childrenIds)) {
		    //var startTime = window.performance.now();
		    var text = templateFn(val.Fields);
		    //var endTime = window.performance.now();
		    //console.log("doT time: " + (endTime - startTime) + "ms");
		    allText.push(text);
		}
	    } catch(e) {
		console.log(e);
	    }
	});
	//startTime = window.performance.now();
	node.empty();
	node.append(allText.join(' '));
	//endTime = window.performance.now();
	//console.log("DOM time: " + (endTime - startTime) + "ms");
    }

    function processBibtex2JSONPubs(jsonResult) {
	var me = "Begel, Andrew";
	
	$.each(jsonResult.entries, function(key, val) {
	    try {
		if ("author" in val.Fields) {
		    var authors = val.Fields['author'];
		    var highlightedMe = authors.replace(me, "\<b\>" + me + "\<\/b\>");
		    val.Fields['author'] = highlightedMe;
		} else if ("editor" in val.Fields) {
		    var authors = val.Fields['editor'];
		    var highlightedMe = authors.replace(me, "\<b\>" + me + "\<\/b\>");
		    val.Fields['editor'] = highlightedMe;
		} else {
		    val.Fields['author'] = "";
		}
		var topicCode = val.Fields['topicCode'];
		var topicCodes = topicCode.split(" ");
		var topics = [];
		for(var i = 0; i < topicCodes.length; i++) {
		    var topicAndLabel = encodeTopicAndLabel(topicCodes[i]);
		    topics.push(topicAndLabel);
		}
		val.Fields['topics'] = topics;
		
		if ("abstract" in val.Fields) {
		    val.Fields['abstract'] = urlify(val.Fields['abstract']);
		}
		
	    } catch (e) {
		console.log("oops");
	    }
	});

	// sort entries on initial load
	jsonResult.entries = jsonResult.entries.sort(comparePubs);
    }

    function processPublications(data, templateFn) {
	var jsonResult = BibtexParser(data);
	processBibtex2JSONPubs(jsonResult);
	
	modifyDOMforPubs(jsonResult, templateFn);
	
	InitializePublications();

	return jsonResult;
    }

    $.ajax({
	type: 'GET',
	headers: { "Access-Control-Request-Headers": "Last-Modified" },
	url: 'papers/pubs.bib',
	error: function(data, response, errorString) {
	    console.log("failed to fetch publication list");
	},
	success: function(data, response, xhr) {
	    if (Modernizr.localstorage) {
		var storageLastModified = localStorage["publicationsDate"];
		if (storageLastModified != null) {
		    var pubsLastModified = xhr.getResponseHeader('Last-Modified');
		    if (pubsLastModified != null && 
			new Date(pubsLastModified) <= new Date(storageLastModified)) {
			// use the cached copy
			try {
			    var processedJSON = JSON.parse(localStorage["publications"]);
			    modifyDOMforPubs(processedJSON, templateFn);
			    InitializePublications();
			} 
			catch (e) {
			    // bad data in cache. update it.
			    var jsonResult = processPublications(data, templateFn);
			    localStorage["publications"] = JSON.stringify(jsonResult);
			    localStorage["publicationsDate"] = new Date();	
			}
		    } else {
			// stale pubs. update the cache
			var jsonResult = processPublications(data, templateFn);
			localStorage["publications"] = JSON.stringify(jsonResult);
			localStorage["publicationsDate"] = new Date();		    
		    }
		} else {
		    // no stored pubs. update the cache
		    var jsonResult = processPublications(data, templateFn);
		    localStorage["publications"] = JSON.stringify(jsonResult);
		    localStorage["publicationsDate"] = new Date();	    
		}
	    } else {
		// no local storage. always. process the data
		processPublications(data, templateFn);
	    }
	}
    });


	/*++++++++++++++++++++++++++++++++++++
		ul-withdetails details show/hide
	++++++++++++++++++++++++++++++++++++++*/
	$("ul.ul-withdetails li").find(".row").on('click',function(){
		// $this = $(this);
		$(this).closest("li").find(".details")
	        .stop(true, true)
	        .animate({
	            height:"toggle",
	            opacity:"toggle"
	        },300);
	}).on('mouseenter',function(){
		$this = $(this);
		var anim = new TweenLite($(this).closest("li").find(".imageoverlay"),0.4,{left:0});
	}).on('mouseleave', function(){
		var anim = new TweenLite($(this).closest("li").find(".imageoverlay"),0.2,{left:"-102%"});
	});




	/*++++++++++++++++++++++++++++++++++++
		gallery overlays and popups
	++++++++++++++++++++++++++++++++++++++*/ 

	$(".grid").on("mouseenter","li",function(){
		new TweenLite($(this).find(".over"),0.4,{bottom:0,top:0});
	}).on("mouseleave","li",function(){
		new TweenLite($(this).find(".over"),0.4,{bottom:"-100%", top:"100%"});
	});

	$('.popup-with-move-anim').magnificPopup({
		type: 'image',

		fixedContentPos: false,
		fixedBgPos: true,

		overflowY: 'auto',

		closeBtnInside: true,
		preloader: false,
		
		midClick: true,
		removalDelay: 400,
		mainClass: 'my-mfp-slide-bottom'
	});

    /* Detect touch devices*/
	function isTouchSupported(){
        //check if device supports touch
        var msTouchEnabled = window.navigator.msMaxTouchPoints;
        var generalTouchEnabled = "ontouchstart" in document.createElement("div");
     
        if (msTouchEnabled || generalTouchEnabled) {
            return true;
        }
        return false;

  	}


});


$(window).on("load", function(){

	/*++++++++++++++++++++++++++++++++++++
		gallery masonry layout
	++++++++++++++++++++++++++++++++++++++*/
	var $container = $('#grid');
	// initialize
	$container.masonry({
	  itemSelector: 'li'
	});
	
});

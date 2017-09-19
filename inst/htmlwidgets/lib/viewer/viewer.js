/*
	EBImage JavaScript Image Viewer
	Copyright (c) 2012 Andrzej Oles
*/

function Viewer(parent){
  var viewer = this;
	var data = null;
  var numberOfFrames = 0;

  var originalWidth 	= 0;
  var originalHeight	= 0;

	var currentFrame = 1;
	var zoomLevel = null, minZoomLevel = -12, maxZoomLevel = 6; // 'zoomLevel == 0' is 100%, 'zoomLevel == null' is autofit
	var previousMousePosition = null;
	var baseMouseSpeed = currentMouseSpeed = minMouseSpeed = 2;

	// viever DOM elements
	var body = document.getElementById(parent);
	var canvas = null;
	var image = null;
	var toolbar = null;
	var statusbar = null;
	var help = null;
	var buttons = [];
	var status = [];
	
	var navEnabled = {
	  first: "<img src='data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij4NCiAgPHBvbHlnb24gcG9pbnRzPSIxNiw0IDksOCAxNiwxMiIgc3R5bGU9ImZpbGw6YmxhY2s7Ii8+DQogIDxwb2x5Z29uIHBvaW50cz0iOSw0IDIsOCA5LDEyIiBzdHlsZT0iZmlsbDpibGFjazsiLz4NCiAgPHBvbHlnb24gcG9pbnRzPSIyLDQgMSw0IDEsMTIgMiwxMiIgc3R5bGU9ImZpbGw6YmxhY2s7Ii8+DQo8L3N2Zz4='>",
	  prev: "<img src='data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij4NCiAgPHBvbHlnb24gcG9pbnRzPSIxMSw0IDQsOCAxMSwxMiIgc3R5bGU9ImZpbGw6YmxhY2s7Ii8+DQo8L3N2Zz4='>",
	  next: "<img src='data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij4NCiAgPHBvbHlnb24gcG9pbnRzPSI1LDQgMTIsOCA1LDEyIiBzdHlsZT0iZmlsbDpibGFjazsiLz4NCjwvc3ZnPg=='>",
	  last: "<img src='data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij4NCiAgPHBvbHlnb24gcG9pbnRzPSIwLDQgNyw4IDAsMTIiIHN0eWxlPSJmaWxsOmJsYWNrOyIvPg0KICA8cG9seWdvbiBwb2ludHM9IjcsNCAxNCw4IDcsMTIiIHN0eWxlPSJmaWxsOmJsYWNrOyIvPg0KICA8cG9seWdvbiBwb2ludHM9IjE0LDQgMTUsNCAxNSwxMiAxNCwxMiIgc3R5bGU9ImZpbGw6YmxhY2s7Ii8+DQo8L3N2Zz4='>"
	};
	var navDisabled = {
	  first:
	"<img src='data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij4NCiAgPHBvbHlnb24gcG9pbnRzPSIxNiw0IDksOCAxNiwxMiIgc3R5bGU9ImZpbGw6IzgwODA4MDsiLz4NCiAgPHBvbHlnb24gcG9pbnRzPSI5LDQgMiw4IDksMTIiIHN0eWxlPSJmaWxsOiM4MDgwODA7Ii8+DQogIDxwb2x5Z29uIHBvaW50cz0iMiw0IDEsNCAxLDEyIDIsMTIiIHN0eWxlPSJmaWxsOiM4MDgwODA7Ii8+DQo8L3N2Zz4='>",
	  prev: "<img src='data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij4NCiAgPHBvbHlnb24gcG9pbnRzPSIxMSw0IDQsOCAxMSwxMiIgc3R5bGU9ImZpbGw6IzgwODA4MDsiLz4NCjwvc3ZnPg=='>",
	  next: "<img src='data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij4NCiAgPHBvbHlnb24gcG9pbnRzPSI1LDQgMTIsOCA1LDEyIiBzdHlsZT0iZmlsbDojODA4MDgwOyIvPg0KPC9zdmc+'>",
	  last: "<img src='data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgd2lkdGg9IjE2IiBoZWlnaHQ9IjE2Ij4NCiAgPHBvbHlnb24gcG9pbnRzPSIwLDQgNyw4IDAsMTIiIHN0eWxlPSJmaWxsOiM4MDgwODA7Ii8+DQogIDxwb2x5Z29uIHBvaW50cz0iNyw0IDE0LDggNywxMiIgc3R5bGU9ImZpbGw6IzgwODA4MDsiLz4NCiAgPHBvbHlnb24gcG9pbnRzPSIxNCw0IDE1LDQgMTUsMTIgMTQsMTIiIHN0eWxlPSJmaWxsOiM4MDgwODA7Ii8+DQo8L3N2Zz4='>"
	};
	
/* Internal functions */

	this.setImagePosition = function(x, y) {
		image.style.left=(Math.round(x)+'px');
		image.style.top=(Math.round(y)+'px');
	}
	this.getImagePosition = function() {
		return [stripUnits(image.style.left,'px'), stripUnits(image.style.top,'px')];
	}

	//return zoom factor given a zoom level
	this.getZoomFactor = function(zoomLevel) {
		if(typeof zoomLevel=='undefined')
			zoomLevel=zoomLevel;
		if(zoomLevel==null)
			return image.width/originalWidth;
		else if(zoomLevel>=0)
			// increase by doubling the size
			return Math.pow(2, zoomLevel);
		else
			// decrease each time by 1/3
			return Math.floor(100 * Math.pow(2, Math.ceil(zoomLevel/2) ) * Math.pow(3/2, zoomLevel%2) ) /100;
	}

	this.zoom = function(dir, x, y) { // direction = 1 (zoom in) or -1 (zoom out)
		if(zoomLevel==null){
			zoomLevel = 0;
			var currentZoomFactor = image.width/originalWidth;

			// find from above
			if(currentZoomFactor > 1)
				while(viewer.getZoomFactor(zoomLevel)<currentZoomFactor) zoomLevel++;
			else
				while(viewer.getZoomFactor(zoomLevel-1)>=currentZoomFactor) zoomLevel--;
			if ( (viewer.getZoomFactor(zoomLevel)!=currentZoomFactor) && (dir==1) )
				zoomLevel--;
		}
		viewer.zoomTo(zoomLevel+dir, x, y);
	}

	this.zoomTo = function(newZoomLevel, x, y) {
		// valid range?
		if( newZoomLevel<minZoomLevel || newZoomLevel>maxZoomLevel )
			return false;
		// within the canvas?
		if( x<0 || y<0 || x>=canvas.clientWidth || y>=canvas.clientHeight )
			return false;

		var zoomFactor = viewer.getZoomFactor(newZoomLevel);
		var size = [originalWidth * zoomFactor, originalHeight * zoomFactor];
		var position = viewer.getImagePosition();

		position[0]-=((x-position[0])*((size[0]/image.width)-1));
		position[1]-=((y-position[1])*((size[1]/image.height)-1));

		viewer.updateImage(size[0], size[1], position[0], position[1], newZoomLevel);

		// button locking
		buttons['in'].disable(newZoomLevel==maxZoomLevel);
		buttons['out'].disable(newZoomLevel==minZoomLevel);
		buttons['org'].disable(viewer.getZoomFactor()==1);
		buttons['fit'].disable(false).style.color='';

		return true;
	}

	this.resetZoom = function() {
		zoomLevel = null;

		// scale image down when its dimensions exceed canvas size
		var canvasSize = [canvas.clientWidth, canvas.clientHeight];
		var downscale = ( canvasSize[0]>originalWidth && canvasSize[1]>originalHeight ) ? false : true;
		var imageSize = downscale ? viewer.fitToCanvas() : [originalWidth, originalHeight];

		viewer.updateImage(imageSize[0], imageSize[1], 0, 0, zoomLevel);

		// lock buttons
		buttons['fit'].disable(true).style.color='#FF0000';
		buttons['org'].disable(!downscale);
	}

	this.updateImage = function(w, h, x, y, newZoomLevel) {
		// set image size
		image.width = Math.round(w);
		image.height = Math.round(h);

		// set image position
		var position = viewer.centerImage(w, h, x ,y);
		viewer.setImagePosition(position[0], position[1]);

		zoomLevel = newZoomLevel;
		baseMouseSpeed = currentMouseSpeed = (viewer.getZoomFactor() > minMouseSpeed) ? Math.round(viewer.getZoomFactor()) : minMouseSpeed;
		viewer.updateStatusField("Zoom", Math.round( 100 * viewer.getZoomFactor() )+'%');
	}

	this.centerImage = function(w, h, x, y) {
		var canvasSize = [canvas.clientWidth, canvas.clientHeight];

		if(w<=canvasSize[0])
			x = Math.round((canvasSize[0] - w)/2);
		if(h<=canvasSize[1])
			y = Math.round((canvasSize[1] - h)/2);

		if(w>canvasSize[0]) {
			if(x>0)
				x=0;
			else if((x+w)<canvasSize[0])
				x=canvasSize[0]-w;
		}

		if(h>canvasSize[1]) {
			if(y>0)
				y=0;
			else if((y+h)<canvasSize[1])
				y=canvasSize[1]-h;
		}

		return [x,y];
	}

	this.fitToCanvas = function() {
		var canvasSize = [canvas.clientWidth, canvas.clientHeight];

		var newWidth = canvasSize[0];
		var newHeight = Math.round((newWidth*originalHeight)/originalWidth);
		if(newHeight>(canvasSize[1])) {
			newHeight = canvasSize[1];
			newWidth = Math.round((newHeight*originalWidth)/originalHeight);
		}
		return [newWidth,newHeight];
	}

	this.moveImage = function(x, y) {
		var position = viewer.getImagePosition();
		position = viewer.centerImage(image.width, image.height, position[0]+x, position[1]+y);
		viewer.setImagePosition(position[0], position[1]);
	}

	this.resetCanvas = function(){
		// recalculate canvas size
		var windowSize = [body.clientWidth, body.clientHeight];//getWindowSize();
		var newCanvasSize = [windowSize[0], windowSize[1] - (toolbar.offsetHeight+statusbar.offsetHeight)];
		// set new canvas size
		canvas.style.width = (Math.round(newCanvasSize[0])+'px');
		canvas.style.height = (Math.round(newCanvasSize[1])+'px');

		// redraw image on canvas
		if(zoomLevel == null)
			viewer.autofitImage();
		else
			viewer.zoomTo(zoomLevel, canvas.clientWidth/2, canvas.clientHeight/2);
	}

	this.setFrame = function(frame) {
		if(typeof frame=='undefined')
			frame = 1;
		if( frame<1 || frame>numberOfFrames )
			return false;

		image.src = data[(frame-1)];

		currentFrame = frame;
		viewer.updateStatusField("Frame", currentFrame+'/'+numberOfFrames);

		// button locking
		buttons['first'].disable(currentFrame==1);
		buttons['prev'].disable(currentFrame==1);
		buttons['next'].disable(currentFrame==numberOfFrames);
		buttons['last'].disable(currentFrame==numberOfFrames);

		return true;
	}

	this.updateStatusField = function(name, value) {
		status[name].innerHTML = name+': '+value+'&nbsp;';
	}

	this.clearStatusField = function(name) {
		status[name].innerHTML = '';
	}

	this.getPixelPosition = function(event){
		var mousePos = getMouseXY(event);
		var imagePos = getObjectXY(image);
		var zoomFactor = viewer.getZoomFactor();
		return [Math.floor((mousePos[0]-imagePos[0])/zoomFactor) + 1, Math.floor((mousePos[1]-imagePos[1])/zoomFactor) + 1];
	}

	this.updatePixelPosition = function(event) {
		event = preProcessEvent(event);

		var pixelPos = viewer.getPixelPosition(event);
		(pixelPos[0]<1 || pixelPos[1]<1 || pixelPos[0]>originalWidth || pixelPos[1]>originalHeight) ? viewer.clearStatusField("Position") : viewer.updateStatusField("Position", '('+pixelPos+')');
	}

	this.clearPixelPosition = function(event) {
		event = preProcessEvent(event);

		viewer.clearStatusField('Position');
	}

/* Helper functions */

	getWindowSize = function() {
		var winW = 630, winH = 460;
		if (body && body.offsetWidth) {
			winW = body.clientWidth;
			winH = body.clientHeight;
		}
		return [winW,winH];
	}

	getObjectXY = function(object) {
		var left = 0, top = 0;
		if (object.offsetParent)
			do {
				left += object.offsetLeft;
				top += object.offsetTop;
			}
			while (object = object.offsetParent);
		return [left, top];
	}

	getMouseXY = function(event) {
		var posX = 0, posY = 0;
		if (!event) event = window.event;	//firefox
		if (event.pageX || event.pageY) {
			posX = event.pageX;
			posY = event.pageY;
		}
		else if (event.clientX || event.clientY) {	//IE
			posX = event.clientX + document.body.scrollLeft
				+ document.documentElement.scrollLeft;
			posY = event.clientY + document.body.scrollTop
				+ document.documentElement.scrollTop;
		}
		return [posX,posY];
	}

	stripUnits = function(string, units) {
		if(typeof string=='number')
			return string;
		var result = string.indexOf(units);
		return parseInt(string.substring(0, (result!=-1) ? result : string.length));
	}

/* User actions */

	// frame navigation
	this.firstFrame = function() {
		viewer.setFrame(1);
	}
	this.prevFrame = function() {
		viewer.setFrame(currentFrame-1);
	}
	this.nextFrame = function() {
		viewer.setFrame(currentFrame+1);
	}
	this.lastFrame = function() {
		viewer.setFrame(numberOfFrames);
	}
	// zooming
	this.zoomIn = function() {
		viewer.zoom(+1, canvas.clientWidth/2, canvas.clientHeight/2);
	}
	this.zoomOut = function() {
		viewer.zoom(-1, canvas.clientWidth/2, canvas.clientHeight/2);
	}
	this.originalSize = function() {
		viewer.zoomTo(0, canvas.clientWidth/2, canvas.clientHeight/2);
	}
	this.autofitImage = function() {
		viewer.resetZoom();
	}
	// help
	this.showHelp = function() {
		help.style.display = 'block';
	}
	this.hideHelp = function() {
		help.style.display = 'none';
	}

/* Mouse and keyboard actions */

	setUpMouseWheelAction = function(action) {
		if (body.addEventListener) {
    	// IE9, Chrome, Safari, Opera
    	body.addEventListener("mousewheel", action, false);
  	  // Firefox
    	body.addEventListener("DOMMouseScroll", action, false);
    }
    // IE 6/7/8
    else body.attachEvent("onmousewheel", action);
	}

	preProcessEvent = function(e) {
		if (!e) //For IE
			e = window.event, e.returnValue = false;
		else if (e.preventDefault)
			e.preventDefault();

		return e;
	}

	this.onmousewheel = function(event) {
		event = preProcessEvent(event);

		var direction = 0; // up is +, down is -

		if (event.wheelDelta) 	// IE & Chrome
			direction = (event.wheelDelta > 1) ? 1 : -1;
		else if (event.detail) // FF & Opera
			direction = (event.detail < 1) ? 1 : -1;

		var mousePos = getMouseXY(event);
		var canvasPos = getObjectXY(canvas);
		viewer.zoom(direction, mousePos[0]-canvasPos[0], mousePos[1]-canvasPos[1]);

		return false;
	}

	this.grabImage =  function(event) {
		event = preProcessEvent(event);

		// set mouse cursor
		var imageSize = [image.width, image.height], canvasSize = [canvas.clientWidth, canvas.clientHeight];
		var cursor = 'crosshair';
		if(imageSize[0]>canvasSize[0] && imageSize[1]>canvasSize[1])
			cursor = 'move';
		else if(imageSize[0]>canvasSize[0])
			cursor = 'e-resize';
		else if(imageSize[1]>canvasSize[1])
			cursor = 'n-resize';

		image.style.cursor = cursor;
		previousMousePosition = getMouseXY(event);
		image.onmousemove = viewer.dragImage;
	}

	this.releaseImage = function(event) {
		event = preProcessEvent(event);

		image.style.cursor = 'crosshair';
		image.onmousemove = null;
	}

	this.dragImage = function(event) {
		event = preProcessEvent(event);

		var mousePosition = getMouseXY(event);
		viewer.moveImage(mousePosition[0]-previousMousePosition[0], mousePosition[1]-previousMousePosition[1]);
		previousMousePosition = mousePosition;
	}

	this.onkeydown = function(event) {

		event = preProcessEvent(event);
		var keyCode = event.which || event.keyCode;

		var shift = [0, 0];

		switch(keyCode){

		// browsing
			// next frame
			case 33: // PageUp
			case 190: // . >
				viewer.nextFrame();
				break;
			// previous frame
			case 34: // PageDown
			case 188: // , <
				viewer.prevFrame();
				break;
			// last frame
			case 35: // End
			case 191: // / ?
				viewer.lastFrame();
				break;
			// first frame
			case 36: // Home
			case 77: // m
				viewer.firstFrame();
				break;
		// zooming
			// zoom in
			case 88: // x
			case 43: // + / Numpad +
			case 61:
			case 107:
			case 187:
				viewer.zoomIn();
				break;
			// zoom out
			case 90: // z
			case 45: // - / Numpad -
			case 109:
			case 173:
			case 189:
				viewer.zoomOut();
				break;
			// reset zoom to 100%
			case 82: // r
			case 8: // Backspace
				viewer.originalSize();
				break;
			// fit-in
			case 32: // Space
			case 13: // Enter
				viewer.autofitImage();
				break;
		// moving
			case 37: // Left arrow
				shift[0] += currentMouseSpeed;
				break;
			case 38: // Up arrow
				shift[1] += currentMouseSpeed;
				break;
			case 39: // Right arrow
				shift[0] -= currentMouseSpeed;
				break;
			case 40: // Down arrow
				shift[1] -= currentMouseSpeed;
				break;
		// help
			// show
			case 72: // h
				viewer.showHelp();
				break;
			// hide
			case 27: // Esc
			case 81: // q
				viewer.hideHelp();
				break;
		// debug
			case 68: // d
				break;
		}

		if( keyCode>=37 && keyCode<=40){ // when moving
			viewer.moveImage(shift[0], shift[1]);
			currentMouseSpeed += baseMouseSpeed;
		}
	}

	this.onkeyup = function(event) {
		currentMouseSpeed = baseMouseSpeed;
	}

/* DOM elements creation */

	createElement = function(type, className, parent) {
		var element = document.createElement(type);
		element.className = "ebimage-" + className;
		parent.appendChild(element);
		return element;
	}

	createButton = function(name, value, title, onclick, group) {
		var button = createElement('button', "button", group);
		button.className += " btn-" + name;
		button.innerHTML = value;
		button.title = title;
		button.onclick = onclick;
		button.disable = function(disable){this.disabled=disable; this.blur(); return this;};
		return (buttons[name] = button);
	}

  createNavButton = function(name, title, onclick) {
		var button = createElement('button', "button", navbuttons);
		button.className += " btn-" + name;
		button.title = title;
		button.onclick = onclick;
		button.disable = function(disable) {
		  this.disabled = disable;
		  this.innerHTML = disable ? navDisabled[name] : navEnabled[name];
		  this.blur();
		  return this;
		};
		return (buttons[name] = button);
	}

	createStatusField = function(name) {
	  var statusField = createElement('div', 'status ebimage', statusbar)
	  statusField.className += " sts-" + name;
		return (status[name] = statusField);
	}

/* Set image */
  this.reset = function(img, width, height) {
    if (!(img instanceof Array)) {
        data = [img];
      } else {
        data = img;
      }

    numberOfFrames = data.length;

    originalWidth 	= width;
  	originalHeight	= height;

  	// set up image
		viewer.setFrame();
		image.onload = viewer.resetCanvas();

		viewer.updateStatusField("Image", originalWidth+'x'+originalHeight);
  }

/* Viewer initialization */
	this.init = function() {

		// create toolbar
		toolbar = createElement('div', 'toolbar ebimage', body);
		// create button containers
		navbuttons = createElement('div', 'buttons-nav ebimage', toolbar);
		zoombuttons = createElement('div', 'buttons-zoom ebimage', toolbar);

		// create navigation buttons
		createNavButton('first','First frame [HOME]/[m] ',viewer.firstFrame);
		createNavButton('prev','Previous frame [PAGE DOWN]/[<]',viewer.prevFrame);
		createNavButton('next','Next frame [PAGE UP]/[>]',viewer.nextFrame);
		createNavButton('last','Last frame [END]/[?]',viewer.lastFrame);

		// create zoom buttons
		createButton('in','+','Zoom in [+]/[x]', viewer.zoomIn, zoombuttons);
		createButton('out','&#8722;','Zoom out [-]/[z]',viewer.zoomOut, zoombuttons);
		createButton('org','1:1','Original size [BACKSPACE]/[r]',viewer.originalSize, zoombuttons);
		createButton('fit','&#8727;<br/>&nbsp;','Fit image [SPACE]/[ENTER]',viewer.autofitImage, zoombuttons);

		// create canvas
		canvas = createElement('div', 'canvas ebimage', body)

		//create statusbar
		statusbar = createElement('div', 'statusbar ebimage', body);

		// create statusbar elements
		createStatusField("Image"), createStatusField("Frame"), createStatusField("Zoom"), createStatusField("Position");

		// create help
		help = createElement('div', 'help ebimage', canvas)
		help.innerHTML = '<table class="ebimage-help-table"><tr><td colspan="3" class="ebimage-help-topic">Browsing</td></tr><tr><td colspan="3">Use toolbar buttons or the following keys to change between the frames:</td></tr><tr><td>Next frame</td><td class="ebimage-help-key">PAGE UP</td><td class="ebimage-help-key">&gt</td></tr><tr><td>Previous frame</td><td class="ebimage-help-key">PAGE DOWN</td><td class="ebimage-help-key">&lt</td></tr><tr><td>First frame</td><td class="ebimage-help-key">HOME</td><td class="ebimage-help-key">M</td></tr><tr><td>Last frame</td><td class="ebimage-help-key">END</td><td class="ebimage-help-key">?</td></tr><tr><td colspan="3" class="ebimage-help-topic">Zooming</td></tr><tr><td colspan="3">To zoom the image in/out use the mouse wheel, the toolbar buttons, or the following keyboard shortcuts:</td></tr><tr><td>Zoom in</td><td class="ebimage-help-key">+</td><td class="ebimage-help-key">x</td></tr><tr><td>Zoom out</td><td class="ebimage-help-key">-</td><td class="ebimage-help-key">z</td></tr><tr><td>Reset to 100%</td><td class="ebimage-help-key">BACKSPACE</td><td class="ebimage-help-key">R</td></tr><tr><td>Fit-in</td><td class="ebimage-help-key">SPACE</td><td class="ebimage-help-key">ENTER</td></tr><tr><td colspan="3" class="ebimage-help-topic">Panning</td></tr><tr><td colspan="3">To pan the image click on it and drag it with your mouse. Alternatively, use the arrow keys on your keyboard.</td></tr><tr><td colspan="3" class="ebimage-help-close">Press ESC or Q to close this window.</td></tr></table>'

		// create image
		image = createElement('img', 'image ebimage', canvas);

		// set up mouse and keyboard actions
		// use mousewheel for zooming
    setUpMouseWheelAction(viewer.onmousewheel);

		// grab and pan image on click
		image.onmousedown = viewer.grabImage;
		image.onmouseup = image.onmouseout = viewer.releaseImage;
		image.onmousemove = null;
		// read keystrokes, http://jsfiddle.net/erCEq/173/
		body.tabIndex = 0;
		body.style.outline = "none";
		body.onmouseover = function() { this.focus(); };
    body.onmouseout = function() { this.blur(); };
    body.onfocus = function() { this.onkeydown = viewer.onkeydown; };
		// read current pixel position
		canvas.onmousemove = viewer.updatePixelPosition;
		canvas.onmouseout = viewer.clearPixelPosition;

		// reset view on window resize
		//window.onresize = resetCanvas;
	}

	this.onload = viewer.init();
}

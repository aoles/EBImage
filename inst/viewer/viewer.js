/*
	EBImage JavaScript Image Viewer
	Copyright (c) 2012 Andrzej Oles 
*/

function Viewer(image, frames, width, height){
	if (arguments.callee.viewer)
		return arguments.callee.viewer;
	var viewer = arguments.callee.viewer = this;

	var imageName		= image;
	var numberOfFrames	= frames;
	var originalWidth 	= width;
	var originalHeight	= height;

	var currentFrame = 1;
	var zoomLevel = null, minZoomLevel = -12, maxZoomLevel = 6; // 'zoomLevel == 0' is 100%, 'zoomLevel == null' is autofit
	var previousMousePosition = null;
	var baseMouseSpeed = currentMouseSpeed = minMouseSpeed = 2;

	// viever DOM elements
	var body = document.body; 
	var canvas = null, image = null, toolbar = null, statusbar = null, help = null;
	var buttons = [], status = [];


/* Internal functions */

	setImagePosition = function(x, y) {
		image.style.left=(Math.round(x)+'px');
		image.style.top=(Math.round(y)+'px');
	}
	getImagePosition = function() {
		return [stripUnits(image.style.left,'px'), stripUnits(image.style.top,'px')];
	}

	//return zoom factor given a zoom level
	getZoomFactor = function(zoomLevel) {
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

	zoom = function(dir, x, y) { // direction = 1 (zoom in) or -1 (zoom out)
		if(zoomLevel==null){
			zoomLevel = 0;
			var currentZoomFactor = image.width/originalWidth;
		
			// find from above
			if(currentZoomFactor > 1)
				while(getZoomFactor(zoomLevel)<currentZoomFactor) zoomLevel++;
			else
				while(getZoomFactor(zoomLevel-1)>=currentZoomFactor) zoomLevel--;
			if ( (getZoomFactor(zoomLevel)!=currentZoomFactor) && (dir==1) )
				zoomLevel--;
		}	
		zoomTo(zoomLevel+dir, x, y);
	}

	zoomTo = function(newZoomLevel, x, y) {
		// valid range?
		if( newZoomLevel<minZoomLevel || newZoomLevel>maxZoomLevel )
			return false;
		// within the canvas?
		if( x<0 || y<0 || x>=canvas.clientWidth || y>=canvas.clientHeight )
			return false;
		
		var zoomFactor = getZoomFactor(newZoomLevel);
		var size = [originalWidth * zoomFactor, originalHeight * zoomFactor];
		var position = getImagePosition();
		
		position[0]-=((x-position[0])*((size[0]/image.width)-1)), position[1]-=((y-position[1])*((size[1]/image.height)-1));
		
		updateImage(size[0], size[1], position[0], position[1], newZoomLevel);
		
		// button locking		
		buttons['in'].disable(newZoomLevel==maxZoomLevel);
		buttons['out'].disable(newZoomLevel==minZoomLevel);
		buttons['org'].disable(getZoomFactor()==1);
		buttons['fit'].disable(false).style.color='';
		
		return true;
	}

	resetZoom = function() {
		zoomLevel = null;
		
		// scale image down when its dimensions exceed canvas size
		var canvasSize = [canvas.clientWidth, canvas.clientHeight];
		var downscale = ( canvasSize[0]>originalWidth && canvasSize[1]>originalHeight ) ? false : true;
		var imageSize = downscale ? fitToCanvas() : [originalWidth, originalHeight];

		updateImage(imageSize[0], imageSize[1], 0, 0, zoomLevel);

		// lock buttons
		buttons['fit'].disable(true).style.color='#FF0000';
		buttons['org'].disable(!downscale);
	}

	updateImage = function(w, h, x, y, newZoomLevel) {
		// set image size
		image.width = Math.round(w);
		image.height = Math.round(h);

		// set image position
		var position = centerImage(w, h, x ,y);
		setImagePosition(position[0], position[1]);

		zoomLevel = newZoomLevel;
		baseMouseSpeed = currentMouseSpeed = (getZoomFactor() > minMouseSpeed) ? Math.round(getZoomFactor()) : minMouseSpeed;
		updateStatusField("Zoom", Math.round( 100 * getZoomFactor() )+'%');
	}

	centerImage = function(w, h, x, y) { 
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

	fitToCanvas = function() {
		var canvasSize = [canvas.clientWidth, canvas.clientHeight];
		
		var newWidth = canvasSize[0];
		var newHeight = Math.round((newWidth*height)/originalWidth);
		if(newHeight>(canvasSize[1])) {
			newHeight = canvasSize[1];
			newWidth = Math.round((newHeight*width)/originalHeight); 
		}
		return [newWidth,newHeight];
	}

	moveImage = function(x, y) {
		var position = getImagePosition();
		position = centerImage(image.width, image.height, position[0]+x, position[1]+y);
		setImagePosition(position[0], position[1]);
	}

	resetCanvas = function(){
		// recalculate canvas size
		var windowSize = getWindowSize();
		var newCanvasSize = [windowSize[0], windowSize[1] - (toolbar.offsetHeight+statusbar.offsetHeight)];
		// set new canvas size
		canvas.style.width = (Math.round(newCanvasSize[0])+'px');
		canvas.style.height = (Math.round(newCanvasSize[1])+'px');

		// redraw image on canvas
		if(zoomLevel == null)
			viewer.autofitImage();
		else
			zoomTo(zoomLevel, canvas.clientWidth/2, canvas.clientHeight/2);
	}

	setFrame = function(frame) {
		if(typeof frame=='undefined')
			frame = 1;
		if( frame<1 || frame>numberOfFrames )
			return false;

		// determine filename
		var framename = imageName;
		if(numberOfFrames>1){
			filename = imageName.split('.');
			extension = filename.pop();
			framename = filename.join('.')+'-'+(frame-1)+'.'+extension;
		}
		image.src = framename;

		currentFrame = frame;
		updateStatusField("Frame", currentFrame+'/'+numberOfFrames);
		
		// button locking
		buttons['first'].disable(currentFrame==1);
		buttons['prev'].disable(currentFrame==1);
		buttons['next'].disable(currentFrame==numberOfFrames);
		buttons['last'].disable(currentFrame==numberOfFrames);
		
		return true;
	}

	updateStatusField = function(name, value) {
		status[name].innerHTML = name+': '+value+'&nbsp;';
	}

	clearStatusField = function(name) {
		status[name].innerHTML = '';
	}

	getPixelPosition = function(event){
		var mousePos = getMouseXY(event);
		var imagePos = getObjectXY(image);
		var zoomFactor = getZoomFactor();
		return [Math.floor((mousePos[0]-imagePos[0])/zoomFactor), Math.floor((mousePos[1]-imagePos[1])/zoomFactor)];
	}

	updatePixelPosition = function(event) {
		event = preProcessEvent(event);

		var pixelPos = getPixelPosition(event);
		(pixelPos[0]<0 || pixelPos[1]<0 || pixelPos[0]>=originalWidth || pixelPos[1]>=originalHeight) ? clearStatusField("Position") : updateStatusField("Position", '('+pixelPos+')');
	}

	clearPixelPosition = function(event) {
		event = preProcessEvent(event);

		clearStatusField('Position');
	}

/* Helper functions */

	getWindowSize = function() {
		var winW = 630, winH = 460;
		if (document.body && document.body.offsetWidth) {
			winW = document.body.offsetWidth;
			winH = document.body.offsetHeight;
		}
		if (document.compatMode=='CSS1Compat' && document.documentElement && document.documentElement.offsetWidth ) {
			winW = document.documentElement.offsetWidth;
			winH = document.documentElement.offsetHeight;
		}
		if (window.innerWidth && window.innerHeight) {
			winW = window.innerWidth;
			winH = window.innerHeight;
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
	viewer.firstFrame = function() {
		setFrame(1);
	}
	viewer.prevFrame = function() {
		setFrame(currentFrame-1);
	}
	viewer.nextFrame = function() {
		setFrame(currentFrame+1);
	}
	viewer.lastFrame = function() {
		setFrame(numberOfFrames);
	}
	// zooming
	viewer.zoomIn = function() {
		zoom(+1, canvas.clientWidth/2, canvas.clientHeight/2);
	}
	viewer.zoomOut = function() {
		zoom(-1, canvas.clientWidth/2, canvas.clientHeight/2);
	}
	viewer.originalSize = function() {
		zoomTo(0, canvas.clientWidth/2, canvas.clientHeight/2);
	}
	viewer.autofitImage = function() {
		resetZoom();
	}
	// help
	viewer.showHelp = function() {
		help.style.display = 'block';
	}
	viewer.hideHelp = function() {
		help.style.display = 'none';
	}

/* Mouse and keyboard actions */

	setUpMouseWheelAction = function(action) {
			if (window.addEventListener) //For firefox
				window.addEventListener('DOMMouseScroll', action, false);
			//For IE			
			document.onmousewheel = action;
	}

	preProcessEvent = function(e) {
		if (!e) //For IE
			e = window.event, e.returnValue = false;
		else if (e.preventDefault)
			e.preventDefault();

		return e;
	}

	viewer.onmousewheel = function(event) {
		event = preProcessEvent(event);

		var direction = 0; // up is +, down is -
		
		if (event.wheelDelta) 	// IE & Chrome
			direction = (event.wheelDelta > 1) ? 1 : -1;
		else if (event.detail) // FF & Opera
			direction = (event.detail < 1) ? 1 : -1;

		var mousePos = getMouseXY(event);
		var canvasPos = getObjectXY(canvas);
		zoom(direction, mousePos[0]-canvasPos[0], mousePos[1]-canvasPos[1]);
	}
	
	viewer.grabImage =  function(event) {
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

	viewer.releaseImage = function(event) {
		event = preProcessEvent(event);

		image.style.cursor = 'crosshair';
		image.onmousemove = null;
	}

	viewer.dragImage = function(event) {
		event = preProcessEvent(event);
		
		var mousePosition = getMouseXY(event);
		moveImage(mousePosition[0]-previousMousePosition[0], mousePosition[1]-previousMousePosition[1]);
		previousMousePosition = mousePosition;
	}

	viewer.onkeydown = function(event) {
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
			moveImage(shift[0], shift[1]);
			currentMouseSpeed += baseMouseSpeed;
		}
	}

	viewer.onkeyup = function(event) {
		currentMouseSpeed = baseMouseSpeed;
	}

/* DOM elements creation */

	createElement = function(type, id, className, parent) {
		var element = document.createElement(type);
		element.id = id;
		if(className!=null) element.className = className;
		parent.appendChild(element);
		return element;
	}

	createButton = function(name, value, title, onclick, group) {
		var button = createElement('button', 'button_'+name, null, group)
		button.innerHTML = value;
		button.title = title;
		button.onclick = onclick;
		button.disable = function(disable){this.disabled=disable; this.blur(); return this;};

		return (buttons[name] = button);
	}

	createStatusField = function(name) {
		return (status[name] = createElement('div', name, 'status', statusbar));
	}

/* Viewer initialization */

	init = function() {
		
		// create toolbar
		toolbar = createElement('div', 'toolbar', null, body);
		// create button containers
		navbuttons = createElement('div', 'navbuttons', 'buttons', toolbar), zoombuttons = createElement('div', 'zoombuttons', 'buttons', toolbar);
		
		// create navigation buttons
		createButton('first','&#171;<br/>&nbsp;','First frame [HOME]/[m] ',viewer.firstFrame,navbuttons);
		createButton('prev','&lt;','Previous frame [PAGE DOWN]/[<]',viewer.prevFrame,navbuttons);
		createButton('next','&gt;','Next frame [PAGE UP]/[>]',viewer.nextFrame,navbuttons);
		createButton('last','&#187;<br/>&nbsp;','Last frame [END]/[?]',viewer.lastFrame,navbuttons);
		
		// create zoom buttons
		createButton('in','+','Zoom in [+]/[x]',viewer.zoomIn,zoombuttons);
		createButton('out','&#8722;','Zoom out [-]/[z]',viewer.zoomOut,zoombuttons);
		createButton('org','1:1','Original size [BACKSPACE]/[r]',viewer.originalSize,zoombuttons);
		createButton('fit','&#8727;<br/>&nbsp;','Fit image [SPACE]/[ENTER]',viewer.autofitImage,zoombuttons);
		
		// create canvas
		canvas = createElement('div', 'canvas', null, body)
		
		//create statusbar
		statusbar = createElement('div', 'statusbar', null, body);
		
		// create statusbar elements
		createStatusField("Image"), createStatusField("Frame"), createStatusField("Zoom"), createStatusField("Position");
		
		// create help
		help = createElement('div', 'help', null, canvas)
		help.innerHTML = '<table><tr><td colspan="3" class="topic">Browsing</td></tr><tr><td colspan="3">Use toolbar buttons or the following keys to change between the frames:</td></tr><tr><td>Next frame</td><td class="key">PAGE UP</td><td class="key">&gt</td></tr><tr><td>Previous frame</td><td class="key">PAGE DOWN</td><td class="key">&lt</td></tr><tr><td>First frame</td><td class="key">HOME</td><td class="key">M</td></tr><tr><td>Last frame</td><td class="key">END</td><td class="key">?</td></tr><tr><td colspan="3" class="topic">Zooming</td></tr><tr><td colspan="3">To zoom the image in/out use the mouse wheel, the toolbar buttons, or the following keyboard shortcuts:</td></tr><tr><td>Zoom in</td><td class="key">+</td><td class="key">x</td></tr><tr><td>Zoom out</td><td class="key">-</td><td class="key">z</td></tr><tr><td>Reset to 100%</td><td class="key">BACKSPACE</td><td class="key">R</td></tr><tr><td>Fit-in</td><td class="key">SPACE</td><td class="key">ENTER</td></tr><tr><td colspan="3" class="topic">Panning</td></tr><tr><td colspan="3">To pan the image click on it and drag it with your mouse. Alternatively, use the arrow keys on your keyboard.</td></tr><tr><td colspan="3" class="close">Press ESC or Q to close this window.</td></tr></table>'
		
		// create image	
		image = createElement('img', 'ebimage', null, canvas);
		
		// set up image
		setFrame();
		image.onload = resetCanvas;
		
		updateStatusField("Image", originalWidth+'x'+originalHeight);
		
		// set up mouse and keyboard actions
		// use mousewheel for zooming
		setUpMouseWheelAction(viewer.onmousewheel);
		// grab and pan image on click
		image.onmousedown = viewer.grabImage;
		image.onmouseup = image.onmouseout = viewer.releaseImage;
		image.onmousemove = null;
		// read keystrokes
		document.onkeydown = viewer.onkeydown;
		document.onkeyup = viewer.onkeyup;
		// read current pixel position
		canvas.onmousemove = updatePixelPosition;
		canvas.onmouseout = clearPixelPosition;
		
		// reset view on window resize
		window.onresize = resetCanvas;
	}
	
	viewer.onload = init();
}

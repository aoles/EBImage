/**
 *  Simple Javascript Image Viewer
    Copyright (C)	2010  Munawwar Firoz
					2012  Andrzej Oles

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details (http://www.gnu.org/licenses/)
*/

function getWindowSize(){
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
	return [winW,winH]
}

///////////////////////////////////////////////////////////
function getObjectXY(object) {
	var left,top;
	objectCopy=object;
	if (object.offsetParent) {
		left=top=0;
		do {
			left += object.offsetLeft;
			if(object.style.borderLeftWidth!='')
				left+=parseInt(object.style.borderLeftWidth);
			else
				object.style.borderLeftWidth='0px';
			top += object.offsetTop;
			if(object.style.borderTopWidth!='')
				top+=parseInt(object.style.borderTopWidth);
			else
				object.style.borderTopWidth='0px';
		}
		while (object = object.offsetParent);
	}
	return [left-parseInt(objectCopy.style.borderLeftWidth),top-parseInt(objectCopy.style.borderTopWidth)];
}

function stripUnits(string, units) {
	if(typeof string=='number')
		return string;
	var result = string.indexOf(units);
	return parseInt(string.substring(0, (result!=-1) ? result : string.length))
}

/*Mouse related functions*/
// Used to retrieve the mouse cursor position on screen. Position is relative to top-left point of document area.
function getMouseXY(event) {
	var posx = 0, posy = 0;
	if (!event) event = window.event;	//firefox
	if (event.pageX || event.pageY) {
		posx = event.pageX;
		posy = event.pageY;
	}
	else if (event.clientX || event.clientY) {	//IE
		posx = event.clientX + document.body.scrollLeft
			+ document.documentElement.scrollLeft;
		posy = event.clientY + document.body.scrollTop
			+ document.documentElement.scrollTop;
	}
	return [posx,posy];
}	

function Viewer(image, frames, width, height){
	var viewer=this;

	var parent		= document.getElementById('viewer');
	var imageName		= image;
	var numberOfFrames	= frames;
	var originalWidth 	= width;
	var originalHeight	= height;
	
	// viever DOM elements

	var canvas = null, image=null;
	var toolbar=null, statusbar = null, buttons = [], status = [], help = null;

	var currentFrame = 1;
	var zoomLevel=null, minZoomLevel=-12, maxZoomLevel=6; // zoomLevel == 0 is 100%, zoomLevel == null for fit into frame
	var previousMousePosition = null;
	var baseMouseSpeed = currentMouseSpeed = minMouseSpeed = 2;

	/*Methods*/

	viewer.setImagePosition = function(x,y) { //x and y coordinate of image
		var imageStyle = image.style;
		imageStyle.left=(Math.round(x)+'px');
		imageStyle.top=(Math.round(y)+'px');
	}
	viewer.getImagePosition = function() {
		return [stripUnits(image.style.left,'px'),stripUnits(image.style.top,'px')];
	}
	viewer.setMouseCursor = function() {
		var imageSize = [image.width, image.height];
		var canvasSize = [canvas.clientWidth, canvas.clientHeight];
		
		var cursor='crosshair';
		if(imageSize[0]>canvasSize[0] && imageSize[1]>canvasSize[1])
			cursor='move';
		else if(imageSize[0]>canvasSize[0])
			cursor='e-resize';
		else if(imageSize[1]>canvasSize[1])
			cursor='n-resize';
		
		image.style.cursor=cursor;
	}
	viewer.fitToWindow = function(width, height) { //width and height of image
		if(typeof width=='undefined' || typeof height=='undefined') {
			width=originalWidth, height=originalHeight;
		}
		var canvasSize = [canvas.clientWidth, canvas.clientHeight];
		
		var newWidth = canvasSize[0];
		var newHeight = Math.round((newWidth*height)/width);
		if(newHeight>(canvasSize[1])) {
			newHeight = canvasSize[1];
			newWidth = Math.round((newHeight*width)/height); 
		}
		return [newWidth,newHeight];
	}
	//return zoom factor given a zoom level
	viewer.zoomFactor = function(zoomLevel) {
		if(typeof zoomLevel=='undefined')
			zoomLevel=viewer.zoomLevel;
		if(zoomLevel==null)
			return image.width/originalWidth;
		else if(zoomLevel>=0)
			// increase by doubling the size
			return Math.pow(2, zoomLevel);
		else
			// decrease each time by 1/3 
			return Math.floor(100 * Math.pow(2, Math.ceil(zoomLevel/2) ) * Math.pow(3/2, zoomLevel%2) ) /100;
	}
	
	viewer.zoom = function(dir, x, y) { // direction = 1 (zoom in) or -1 (zoom out)
		if(zoomLevel==null){
			zoomLevel = 0;
			var currentZoomFactor = image.width/originalWidth;
			
			// find from above			
			if(currentZoomFactor > 1)
				while(viewer.zoomFactor(zoomLevel)<currentZoomFactor) zoomLevel++;
			else
				while(viewer.zoomFactor(zoomLevel-1)>=currentZoomFactor) zoomLevel--;
			if ( (viewer.zoomFactor(zoomLevel)!=currentZoomFactor) && (dir==1) )
				zoomLevel--;
		}	
		viewer.zoomTo(zoomLevel+dir, x, y);
	}
	viewer.zoomTo = function(newZoomLevel, x, y) {
		// valid range?
		if( newZoomLevel<minZoomLevel || newZoomLevel>maxZoomLevel )
			return false;
		
		//check if x and y coordinate is within the canvas
		var canvasSize = [canvas.clientWidth, canvas.clientHeight];
		if( x<0 || y<0 || x>=canvasSize[0] || y>=canvasSize[1] )
			return false;
		
		var zoomFactor = viewer.zoomFactor(newZoomLevel);
		var dimension = [originalWidth * zoomFactor, originalHeight * zoomFactor];
		
		//Calculate percentage increase/decrease and fix the image over given x,y coordinate
		var position = viewer.getImagePosition();
		
		//The Maths
		/*
			New point/Old point = New image width/Old image width
		=>	New point = New width/Old width * Old point
			
			Difference between new and old point 
			= New point - Old point
			= New width/Old width * Old point - Old point
			= Old Point * (New width/Old width - 1)
			
			Moving the image by this difference brings the zoomed image to the same (pivot) point.
			
			The point (x,y) sent into this function is relative to the canvas. However, it should be relative to the image for the above formula to work.
			Hence, point = (x-left, y-top).
		*/
		position[0]-=((x-position[0])*((dimension[0]/image.width)-1)), position[1]-=((y-position[1])*((dimension[1]/image.height)-1)); //Applying the above formula
		
		//Center image
		position = viewer.centerImage(dimension[0],dimension[1], position[0],position[1]);
		
		//Set dimension and position
		viewer.updateImage(dimension[0], dimension[1], position[0], position[1], newZoomLevel);

		// button locking		
		buttons['in'].disable(newZoomLevel==maxZoomLevel);
		buttons['out'].disable(newZoomLevel==minZoomLevel);
		buttons['org'].disable(this.zoomFactor()==1);
		buttons['fit'].disable(false).style.color='';
		
		return true;
	}
	viewer.updateImage = function(w, h, x, y, newZoomLevel) {
		// set image size
		image.width=Math.round(w);
		image.height=Math.round(h);
		// set image position
		viewer.setImagePosition(x,y);
		zoomLevel = newZoomLevel;
		viewer.setMouseCursor();
		baseMouseSpeed = currentMouseSpeed = (viewer.zoomFactor() > minMouseSpeed) ? Math.round(viewer.zoomFactor()) : minMouseSpeed;
		viewer.updateStatus("Zoom", Math.round( 100 * viewer.zoomFactor() )+'%');
	}

	viewer.centerImage = function(width,height, x,y) { //width and height of image and (x,y) is the (left,top) of the image
			
		var canvasSize = [canvas.clientWidth, canvas.clientHeight];

		if(width<=canvasSize[0])
			x = Math.round((canvasSize[0] - width)/2);
		if(height<=canvasSize[1])
			y = Math.round((canvasSize[1] - height)/2);

		if(width>canvasSize[0]) {
			if(x>0)
				x=0;
			else
			if((x+width)<canvasSize[0])
				x=canvasSize[0]-width;
		}

		if(height>canvasSize[1]) {
			if(y>0)
				y=0;
			else
			if((y+height)<canvasSize[1])
				y=canvasSize[1]-height;
		}

		return [x,y];
	}
	viewer.resetZoom = function() {
		zoomLevel = null;

		var canvasSize = [canvas.clientWidth, canvas.clientHeight];
		
		// automatically scale image down when its dimensions exceed frame size
		var downscale = ( canvasSize[0]>originalWidth && canvasSize[1]>originalHeight ) ? false : true;
		
		var dimension = downscale ? viewer.fitToWindow(originalWidth,originalHeight) : [originalWidth, originalHeight];
		var position = viewer.centerImage(dimension[0],dimension[1], 0,0);

		viewer.updateImage(dimension[0], dimension[1], position[0], position[1], zoomLevel);

		// lock buttons
		buttons['fit'].disable(true).style.color='#FF0000';
		buttons['org'].disable(!downscale);
	}
	viewer.originalSize = function(){
		viewer.zoomTo(0, canvas.clientWidth/2, canvas.clientHeight/2);
	}
	viewer.autofitImage = function(){
		viewer.resetZoom();
	}

	/*User defined events*/
	//Non-static events
	
	/*Event handlers*/
	setUpMouseWheelAction = function(action){
			if (window.addEventListener) //For firefox
				window.addEventListener('DOMMouseScroll', action, false);
			//For IE			
			document.onmousewheel = action;
	}

	preProcessEvent = function(e){
		if (!e) //For IE
			e = window.event, e.returnValue = false;
		else if (e.preventDefault)
			e.preventDefault();

		return(e);
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
		viewer.zoom(direction, mousePos[0]-canvasPos[0], mousePos[1]-canvasPos[1]);
	}
	viewer.onmousemove = function(event) {
		event = preProcessEvent(event);
		viewer.updateStatus("Position", '('+pixelPosition(event)+')');
	}
	pixelPosition = function(event){
		var mousePos = getMouseXY(event);
		var imagePos = getObjectXY(image);
		var zoomFactor = viewer.zoomFactor();
		return( [Math.floor((mousePos[0]-imagePos[0])/zoomFactor), Math.floor((mousePos[1]-imagePos[1])/zoomFactor)] );
	}
	viewer.moveImage = function(event) {
		event = preProcessEvent(event);
		
		var mousePosition = getMouseXY(event);
		moveBy(mousePosition[0]-previousMousePosition[0], mousePosition[1]-previousMousePosition[1]);
		previousMousePosition = mousePosition;
	}
	moveBy = function(x, y) {
		var position = viewer.getImagePosition();
		position = viewer.centerImage(image.width,image.height, position[0]+x,position[1]+y);
		viewer.setImagePosition(position[0],position[1]);
	}
	viewer.onmouseup_or_out = function(event) {
		event = preProcessEvent(event);
		
		//image.onmousemove=viewer.onmousemove;image.onmouseup=image.onmouseout=null;
		image.onmousemove=viewer.onmousemove;
		image.onmouseup=null;
		image.onmousedown=viewer.onmousedown;
	}
	viewer.onmouseout = function(event) {
		status['Position'].innerHTML ='';
	}
	viewer.onmousedown =  function(event) {
		event = preProcessEvent(event);
	
		previousMousePosition = getMouseXY(event);

		viewer.updateStatus("Position", '('+pixelPosition(event)+')');

		image.onmousemove = viewer.moveImage;
		image.onmouseup=image.onmouseout=viewer.onmouseup_or_out;
	}
	viewer.resetCanvas = function(){
		// recalculate canvas size
		var windowSize = getWindowSize();
		var newCanvasSize = [windowSize[0], windowSize[1] - (toolbar.offsetHeight+statusbar.offsetHeight)];
		// set new canvas size
		var canvasStyle = canvas.style;
		canvasStyle.width = (Math.round(newCanvasSize[0])+'px');
		canvasStyle.height = (Math.round(newCanvasSize[1])+'px');

		// redraw image on canvas
		if(zoomLevel == null)
			viewer.autofitImage();
		else
			viewer.zoomTo(zoomLevel, canvas.clientWidth/2, canvas.clientHeight/2);
	}
	// frame navigation
	this.firstFrame = function(){
		viewer.setFrame(1);
	}
	this.prevFrame = function(){
		viewer.setFrame(currentFrame-1);
	}
	this.nextFrame = function(){
		viewer.setFrame(currentFrame+1);
	}
	this.lastFrame = function(){
		viewer.setFrame(numberOfFrames);
	}
	// zooming
	this.zoomIn = function(){
		viewer.zoom(+1, canvas.clientWidth/2, canvas.clientHeight/2);
	}
	this.zoomOut = function(){
		viewer.zoom(-1, canvas.clientWidth/2, canvas.clientHeight/2);
	}
	// help
	showHelp = function(){
		help.style.display = 'block';
	}
	hideHelp = function(){
		help.style.display = 'none';
	}

	viewer.onkeydown = function(event) {
		event = event || window.event;
		var keyCode = event.which || event.keyCode;
		
		if(event.preventDefault) // Netscape/Firefox/Opera
			event.preventDefault();
		event.returnValue = false;

		image.onload='null';

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
				showHelp();
				break;
			// hide
			case 27: // Esc
			case 81: // q
				hideHelp();
				break;
		}

		if( keyCode>=37 && keyCode<=40){ // when moving
			moveBy(shift[0], shift[1]);
			currentMouseSpeed+=baseMouseSpeed;
		}

	}
	viewer.onkeyup = function(event) {
		currentMouseSpeed = baseMouseSpeed;
	}

	viewer.setFrame = function(frame) {
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
		viewer.updateStatus("Frame", currentFrame+'/'+numberOfFrames);
		
		// button locking
		buttons['first'].disable(currentFrame==1);
		buttons['prev'].disable(currentFrame==1);
		buttons['next'].disable(currentFrame==numberOfFrames);
		buttons['last'].disable(currentFrame==numberOfFrames);
		
		return true;
	}

	createElement = function(type, id, className, parent){
		var element = document.createElement(type);
		element.id = id;
		if(className!=null) element.className = className;
		parent.appendChild(element);
		return(element);
	}

	////////////////// BUTTONS
	createButton = function(name, value, title, onclick, group){
		var button = createElement('button', name, null, group)
		button.innerHTML = value;
		button.title = title;
		button.onclick = onclick;
		button.disable = function(disable){this.disabled=disable; this.blur(); return(this)};

		return(buttons[name] = button);
	}
	createStatusElement = function(name){
		return(status[name] = createElement('div', name, 'status', statusbar));
	}
	viewer.updateStatus = function(name, value){
		status[name].innerHTML = name+': '+value+'&nbsp;';
	}
	///////////////////////////

	init = function(){

		// create toolbar
		toolbar = createElement('div', 'toolbar', null, parent);
		// create button containers
		navbuttons = createElement('div', 'navbuttons', 'buttons', toolbar), zoombuttons = createElement('div', 'zoombuttons', 'buttons', toolbar);
			
		// create navigation buttons
		createButton('first','&#171;<br/>&nbsp;','First frame [HOME] [M] ',viewer.firstFrame,navbuttons);
		createButton('prev','&lt;','Previous frame [PAGE DOWN] [<]',viewer.prevFrame,navbuttons);
		createButton('next','&gt;','Next frame [PAGE UP] [>]',viewer.nextFrame,navbuttons);
		createButton('last','&#187;<br/>&nbsp;','Last frame [END] [?]',viewer.lastFrame,navbuttons);
	
		// create zoom buttons
		createButton('in','+','Zoom in [+] [X]',viewer.zoomIn,zoombuttons);
		createButton('out','&#8722;','Zoom out [-] [Z]',viewer.zoomOut,zoombuttons);
		createButton('org','1:1','Original size [BACKSAPCE] [R]',viewer.originalSize,zoombuttons).style.fontSize='14px';
		createButton('fit','&#8727;<br/>&nbsp;','Fit image [SPACE] [ENTER]',viewer.autofitImage,zoombuttons);
			
		// create image frame
		canvas = createElement('div', 'frame', null, parent)

		// REMOVE STYLE
			canvas.style.display='block'
			canvas.style.border="0px solid #000";
			canvas.style.margin="0px";
			canvas.style.padding="0px";
			canvas.style.overflow="hidden";
			canvas.style.position="relative";
			canvas.style.zIndex=2;
			canvas.tabIndex=1;
			canvas.style.background="black";

		//create statusbar
		statusbar = createElement('div', 'statusbar', null, parent);

		// create statusbar elements
		createStatusElement("Image"), createStatusElement("Frame"), createStatusElement("Zoom"), createStatusElement("Position");

		// create help
		help = createElement('div', 'help', null, canvas)
		help.innerHTML = '<table><tr><td colspan="3" class="topic">Browsing</td></tr><tr><td colspan="3">Use toolbar buttons or the following keys to change between the frames:</td></tr><tr><td>Next frame</td><td class="key">PAGE UP</td><td class="key">&gt</td></tr><tr><td>Previous frame</td><td class="key">PAGE DOWN</td><td class="key">&lt</td></tr><tr><td>First frame</td><td class="key">HOME</td><td class="key">M</td></tr><tr><td>Last frame</td><td class="key">END</td><td class="key">?</td></tr><tr><td colspan="3" class="topic">Zooming</td></tr><tr><td colspan="3">To zoom the image in/out use the mouse wheel, the toolbar buttons, or the following keyboard shortcuts:</td></tr><tr><td>Zoom in</td><td class="key">+</td><td class="key">x</td></tr><tr><td>Zoom out</td><td class="key">-</td><td class="key">z</td></tr><tr><td>Reset to 100%</td><td class="key">BACKSPACE</td><td class="key">R</td></tr><tr><td>Fit-in</td><td class="key">SPACE</td><td class="key">ENTER</td></tr><tr><td colspan="3" class="topic">Panning</td></tr><tr><td colspan="3">To pan the image click on it and drag it with your mouse. Alternatively, use the arrow keys on your keyboard.</td></tr><tr><td colspan="3" class="close">Press ESC or Q to close this window.</td></tr></table>'

		// create image	
		image = createElement('img', 'image', null, canvas);

		// REMOVE STYLE
		image.style.position='absolute';
		image.style.zIndex=3;
		
		// set up image
		viewer.setFrame();
		image.onload = viewer.resetCanvas();

		viewer.updateStatus("Image", originalWidth+'x'+originalHeight);

		// mouse and keyboard actions
		setUpMouseWheelAction(viewer.onmousewheel);
		image.onmousedown = viewer.onmousedown;
		image.onmousemove = viewer.onmousemove;
		image.onmouseout = viewer.onmouseout;
		document.onkeydown = viewer.onkeydown;
		document.onkeyup = viewer.onkeyup;

		// reset view on window resize
		window.onresize = viewer.resetCanvas;		
	}

	
	viewer.onload = init();
}

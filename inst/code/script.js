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

function retInt(str, suffix) {
	if(typeof str=='number')
		return str;
	var result=str.indexOf(suffix);
	return parseInt(str.substring(0,(result!=-1)?result:str.length))
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
	debug.clear();
	debug.println(posx+" "+posy)
	return [posx,posy];
}

function mouseWheel() {
	var self=this;
	/*Event handlers*/
	/*Mouse wheel functions*/

	//Default mouse wheel callback function
	//Variable local to 'this'
	var wheelCallback = function(event,object,delta){
		/*Override this function and write your code there*/
		/*
			delta=-1 when mouse wheel is rolled backwards (towards yourself)
			delta=1 when mouse wheel is rolled forward (away from one's self)
			Note: Here is where you can call the getMouseXY function using the 'event' argument
		*/
	}
	//Mouse wheel event handler
	self.wheelHandler = function (event){
		var delta = 0;
		if (!event) //For IE
			event = window.event;
		if (event.wheelDelta) 	//IE
		{
			delta = event.wheelDelta/120;
			//if (window.opera) delta = -delta; //for Opera...hmm I read somewhere opera 9 need the delta sign inverted...tried in opera 10 and it doesnt require this!?
		}
		else if (event.detail) //firefox
			delta = -event.detail/3;

		if (event.preventDefault)
			event.preventDefault();
		event.returnValue = false;
		if (delta)
			wheelCallback(event,this,delta);	//callback function
	}
	//Mouse wheel initialization
	self.init = function(object,callback) {
		if (object.addEventListener) //For firefox
			object.addEventListener('DOMMouseScroll', this.wheelHandler, false); //Mouse wheel initialization
		//For IE
		object.onmousewheel = this.wheelHandler; //Mouse wheel initialization
		wheelCallback=callback;
	}
	this.setCallback = function(callback){
		wheelCallback=callback;
	}
}

//Debugging
function debug_msgs() {
	this.counter=0;
	this.clear=function() {
		var div=document.getElementById('debug');
		div.innerHTML='';
		this.counter=0;
	}
	this.print=function(string) {
	var div=document.getElementById('debug');
		div.innerHTML+=string;
	}
	this.println=function(string) {
		var div=document.getElementById('debug');
		div.innerHTML+=string+'<br>';
		this.counter++;
	}
}

/*---------------------------*/
// function buttons() //argument array
// {
// 	this.first = "f"
// 	this.prev  = "p"
// 	this.next  = "n"
// 
// 	this.init = function(){
// 		document.getElementById("first").style.color='';
// 		document.getElementById("first").disabled=true;
// 		
// 	}
// 
// }

/*-------------The image viewer--------------*/
function viewer(arguments) //argument array
{
	var self=this;

	/*Properties*/
	//Public access
	self.outerFrame=null;
	
	//Private access
	var image=null,imageSource=null,parent=null,replace=null,preLoader=null;
	var borderClass=null;	
	var originalW, originalH;
	var toolbar=null, statusbar = null, buttons = [], status = [];
	var framesTotal = 1;
	var currentFrame = 1;
	var imageName = null;
	/*Set user defined properties and configurations*/
	/*
		The following configurations are for pure javascript image viewers:
			imageSource : string - Source to the image you want to show
			
			parent : HTMLElement - The parent element of the image viewer
			or
			replace: HTMLElement - The image viewer replace this HTML element
			(Exactly one of the above two properties is absolutly needed)
			
			preLoader (optional) : string - Source to a pre-loader image. Useful in case of large images
			
		The following configurations are for partial javascript/partial HTML image viewers:
			image - HTMLElement - The reference to the image HTML element
		
		Common to both:
			frame - An array of format [width, height, widthIsMax, heightIsMax]
				width and height are strings like '400px' or '100%'. width and height can be in px or %
				widthIsMax and heightIsMax are optional boolean values. 
				Say if widthIsMax is set to true, it treats the width as the maximum width limit.
				So if the zoomed image isn't fitting exactly into the frameElement, the frameElement dimension is reduced and adjusted to fit the image.
	*/
	var key;
	for (key in arguments) {
		var temp=arguments[key];
		eval(key + '=temp;');
	}

	/*Internal states,HTML elements and properties*/
	self.frameElement = null;
	
	var zoomLevel=null, minZoomLevel=-12, maxZoomLevel=6; // zoomLevel == 0 is 100%, zoomLevel == null for fit into frame
	var lastMousePosition=null;
	var baseMouseSpeed = currentMouseSpeed = minMouseSpeed = 2;
	var mouseWheelObject=null;

	/*Methods*/
	self.getFrameDimension =  function() {
		return [self.frameElement.clientWidth,self.frameElement.clientHeight];
	}				
	self.setFrameDimension =  function(width, height) {
		var frameStyle = self.frameElement.style;
		frameStyle.width = (Math.round(width)+'px');
		frameStyle.height = (Math.round(height)+'px');
	}
	self.setDimension = function(width, height) { //width and height of image
		image.width=Math.round(width);
		image.height=Math.round(height);
	}
	self.getDimension =  function() {
		return [image.width,image.height];
	}
	self.setPosition = function(x,y) { //x and y coordinate of image
		image.style.left=(Math.round(x)+'px');
		image.style.top=(Math.round(y)+'px');
	}
	self.getPosition = function() {
		return [retInt(image.style.left,'px'),retInt(image.style.top,'px')];
	}
	self.setMouseCursor = function() {
		var dimension = self.getDimension();
		var frameDimension =  self.getFrameDimension();
		
		var cursor='crosshair';
		if(dimension[0]>frameDimension[0] && dimension[1]>frameDimension[1])
			cursor='move';
		else if(dimension[0]>frameDimension[0])
			cursor='e-resize';
		else if(dimension[1]>frameDimension[1])
			cursor='n-resize';
		
		image.style.cursor=cursor;
	}
	self.fitToFrame = function(width, height) { //width and height of image
		if(typeof width=='undefined' || typeof height=='undefined') {
			width=originalW, height=originalH;
		}
		var frameDimension = self.getFrameDimension(), newWidth,newHeight;
		
		newWidth = frameDimension[0];
		newHeight = Math.round((newWidth*height)/width);
		if(newHeight>(frameDimension[1])) {
			newHeight = frameDimension[1];
			newWidth = Math.round((newHeight*width)/height); 
		}
		return [newWidth,newHeight];
	}
	//return zoom factor given a zoom level
	self.zoomFactor = function(zoomLevel) {
		if(typeof zoomLevel=='undefined')
			zoomLevel=self.zoomLevel;
		if(zoomLevel==null)
			return self.getDimension()[0]/originalW;
		else if(zoomLevel>=0)
			// increase by doubling the size
			return Math.pow(2, zoomLevel);
		else
			// decrease each time by 1/3 
			return Math.floor(100 * Math.pow(2, Math.ceil(zoomLevel/2) ) * Math.pow(3/2, zoomLevel%2) ) /100;
	}
	
	self.zoom = function(dir, x, y) { // direction = 1 (zoom in) or -1 (zoom out)
		if(zoomLevel==null){
			zoomLevel = 0;
			var currentZoomFactor = self.getDimension()[0]/originalW;
			//debug.println("CurrentZoomFactor = "+self.getDimension()[0]+" "+originalW+ " "+currentZoomFactor);
			// find from above			
			if(currentZoomFactor > 1)
				while(self.zoomFactor(zoomLevel)<currentZoomFactor) zoomLevel++;
			else
				while(self.zoomFactor(zoomLevel-1)>=currentZoomFactor) zoomLevel--;
			if ( (self.zoomFactor(zoomLevel)!=currentZoomFactor) && (dir==1) )
				zoomLevel--;
		}	
		self.zoomTo(zoomLevel+dir, x, y);
	}
	self.zoomTo = function(newZoomLevel, x, y) {
		//debug.println("Current zoom = "+zoomLevel+", newZoomLevel = "+newZoomLevel);
		// valid range?
		if( newZoomLevel<minZoomLevel || newZoomLevel>maxZoomLevel )
			return false;
		
		//check if x and y coordinate is within the self.frameElement
		var frameDimension = self.getFrameDimension();
		if( x<0 || y<0 || x>=frameDimension[0] || y>=frameDimension[1] )
			return false;
		
		//var dimension = self.fitToFrame(originalW,originalH);
		var zoomFactor = self.zoomFactor(newZoomLevel);
		debug.println(newZoomLevel+" "+zoomFactor);
		var dimension = [originalW * zoomFactor, originalH * zoomFactor];
		
	
		
		//Calculate percentage increase/decrease and fix the image over given x,y coordinate
		var curWidth=image.width, curHeight=image.height;
		var position = self.getPosition();
		
		//The Maths
		/*
			New point/Old point = New image width/Old image width
		=>	New point = New width/Old width * Old point
			
			Difference between new and old point 
			= New point - Old point
			= New width/Old width * Old point - Old point
			= Old Point * (New width/Old width - 1)
			
			Moving the image by this difference brings the zoomed image to the same (pivot) point.
			
			The point (x,y) sent into this function is relative to the self.frameElement. However, it should be relative to the image for the above formula to work.
			Hence, point = (x-left, y-top).
		*/
		position[0]-=((x-position[0])*((dimension[0]/curWidth)-1)), position[1]-=((y-position[1])*((dimension[1]/curHeight)-1)); //Applying the above formula
		
		
		//Center image
		position = self.centerImage(dimension[0],dimension[1], position[0],position[1]);
		
		//Set dimension and position
		self.updateImage(dimension[0], dimension[1], position[0], position[1], newZoomLevel);
// 		self.setDimension(dimension[0],dimension[1]);
// 		self.setPosition(position[0],position[1]);
// 		self.setMouseCursor();
// 
// 		zoomLevel = newZoomLevel;
// 		self.updateStatus();


		// button locking		
		buttons['in'].disable(newZoomLevel==maxZoomLevel);
		buttons['out'].disable(newZoomLevel==minZoomLevel);
		buttons['org'].disable(newZoomLevel==0);
		buttons['fit'].disable(false);
		
		return true;
	}
	self.updateImage = function(w, h, x, y, newZoomLevel) {
		self.setDimension(w,h);
		self.setPosition(x,y);
		zoomLevel = newZoomLevel;
		self.setMouseCursor();
		baseMouseSpeed = currentMouseSpeed = (self.zoomFactor() > minMouseSpeed) ? Math.round(self.zoomFactor()) : minMouseSpeed;
		self.updateStatus("Zoom", Math.round( 100 * self.zoomFactor() )+'%');
	}

	self.centerImage = function(width,height, x,y) { //width and height of image and (x,y) is the (left,top) of the image
		
		if(typeof width=='undefined' || typeof height=='undefined') {
			var temp = self.getDimension();
			width=temp[0], height=temp[1];
		}
		if(typeof x=='undefined' || typeof y=='undefined') {
			var temp = self.getPosition();
			x=temp[0], y=temp[1];
		}
			
		var frameDimension = self.getFrameDimension();
		
		debug.println("centering..."+width+" "+height+" "+x+" "+y+" "+frameDimension);

		if(width<=frameDimension[0])
			x = Math.round((frameDimension[0] - width)/2);
		if(height<=frameDimension[1])
			y = Math.round((frameDimension[1] - height)/2);

		if(width>frameDimension[0]) {
			if(x>0)
				x=0;
			else
			if((x+width)<frameDimension[0])
				x=frameDimension[0]-width;
		}

		if(height>frameDimension[1]) {
			if(y>0)
				y=0;
			else
			if((y+height)<frameDimension[1])
				y=frameDimension[1]-height;
		}

		return [x,y];
	}
	self.relativeToAbsolute = function(x,y) {
		if(x<0 || y<0 || x>=self.frameElement.clientWidth || y>=self.frameElement.clientHeight)
			return null;
		return [x-retInt(image.style.left,'px'),y-retInt(image.style.top,'px')];
	}
	self.resetZoom = function(zoomLevel) {
		if(typeof zoomLevel=='undefined')
			zoomLevel = null;

		var frameDimension = self.getFrameDimension();
		
		// automatically scale image down when its dimensions exceed frame size
		var dimension = (zoomLevel==null) ? ( ( frameDimension[0]>originalW && frameDimension[1]>originalH ) ? [originalW, originalH] : self.fitToFrame(originalW,originalH) ) : [originalW, originalH];
		
		var position = self.centerImage(dimension[0],dimension[1], 0,0);

		self.updateImage(dimension[0], dimension[1], position[0], position[1], zoomLevel);
	}
	self.originalSize = function(){
		self.resetZoom(0);
	}
	self.fitImage = function(){
		self.resetZoom();
		// lock button
		var button = buttons['fit'];
		button.disabled=true;
		button.style.color='#FF0000';
		button.blur();
	}
	self.moveBy = function(x,y) {
		var position = self.getPosition();
		position = self.centerImage(image.width,image.height, position[0]+x,position[1]+y);
		self.setPosition(position[0],position[1]);
	}
	self.showHelp = function() {
		// hide image
			image.style.display='none';
		//onkeydown = 
		//event = event || window.event;
		//var keyCode = event.which || event.keyCode;
		
		//if(event.preventDefault) // Netscape/Firefox/Opera
		//	event.preventDefault();
		//event.returnValue = false;
	}
	self.hideHelp = function() {
		if(self.outerFrame)
			self.outerFrame.style.display='block';
		else
			self.frameElement.style.display = 'block';
	}

	/*User defined events*/
	//Non-static events
	self.onload = null;
	
	/*Event handlers*/
	self.onmousewheel = function(event,object,direction) {
		self.frameElement.focus();
		if (!event) //For IE
			event=window.event, event.returnValue = false;
		else
		if (event.preventDefault)
			event.preventDefault();
		
			var mousePos = getMouseXY(event);
			var framePos = getObjectXY(self.frameElement);
			self.zoom(direction, mousePos[0]-framePos[0], mousePos[1]-framePos[1]);
	}
	self.onmousemove = function(event) {
		if (!event) //For IE
			event=window.event, event.returnValue = false;
		else if (event.preventDefault)
			event.preventDefault();

		var mousePos = getMouseXY(event);
		var imagePos = getObjectXY(image);
		var zoomFactor = self.zoomFactor();
		var pixelPos = [Math.floor((mousePos[0]-imagePos[0])/zoomFactor), Math.floor((mousePos[1]-imagePos[1])/zoomFactor)]
 
		self.updateStatus("Position", '('+pixelPos+')');
	}
	self.imagemove = function(event) {
		if (!event) //For IE
			event=window.event, event.returnValue = false;
		else if (event.preventDefault)
			event.preventDefault();
		
		var mousePosition=getMouseXY(event);
		var position = self.getPosition();
		position[0]+=(mousePosition[0]-lastMousePosition[0]), position[1]+=(mousePosition[1]-lastMousePosition[1]);
		lastMousePosition=mousePosition;
		
		position = self.centerImage(image.width,image.height, position[0],position[1]);
		self.setPosition(position[0],position[1]);
	}
	self.onmouseup_or_out = function(event) {
		if (!event) //For IE
			event=window.event, event.returnValue = false;
		else
		if (event.preventDefault)
			event.preventDefault();
		
		//image.onmousemove=self.onmousemove;image.onmouseup=image.onmouseout=null;
		image.onmousemove=self.onmousemove;
		image.onmouseup=null;
		image.onmousedown=self.onmousedown;
	}
	self.onmouseout = function(event) {
		status['Position'].innerHTML ='';
	}
	self.onmousedown =  function(event) {
		self.frameElement.focus();
		if (!event) //For IE
			event=window.event, event.returnValue = false;
		else
		if (event.preventDefault)
			event.preventDefault();
	
		var mousePos = lastMousePosition = getMouseXY(event);
		var imagePos = getObjectXY(image);

 		debug.clear();
		debug.println("mouse: " + mousePos + " image: " + imagePos);

		// pixel position

		var zoomFactor = self.zoomFactor();
		var pixelPos = [Math.floor((mousePos[0]-imagePos[0])/zoomFactor), Math.floor((mousePos[1]-imagePos[1])/zoomFactor)]
 
		debug.println("pixel: " + pixelPos + " ZF: " + zoomFactor);

		self.updateStatus("Position", '('+pixelPos+')');
		image.onmousemove = self.imagemove;
		image.onmouseup=image.onmouseout=self.onmouseup_or_out;
	}
	self.resetFrame = function(){
		// set new frame size
		var windowSize = getWindowSize();
		var newFrameSize = [windowSize[0], windowSize[1] - (toolbar.offsetHeight+statusbar.offsetHeight)];
		//debug stuff
		var currentFrameSize = self.getFrameDimension();
		
		self.setFrameDimension(newFrameSize[0], newFrameSize[1]);
		// image redraw
		if(zoomLevel == null)
			self.fitImage();
		else
			self.zoomTo(zoomLevel, self.frameElement.clientWidth/2, self.frameElement.clientHeight/2);
	}
	// frame navigation
	this.firstFrame = function(){
		self.setFrame(1);
	}
	this.prevFrame = function(){
		self.setFrame(currentFrame-1);
	}
	this.nextFrame = function(){
		self.setFrame(currentFrame+1);
	}
	this.lastFrame = function(){
		self.setFrame(framesTotal);
	}
	// zooming
	this.zoomIn = function(){
		self.zoom(+1, self.frameElement.clientWidth/2, self.frameElement.clientHeight/2);
	}
	this.zoomOut = function(){
		self.zoom(-1, self.frameElement.clientWidth/2, self.frameElement.clientHeight/2);
	}
	self.onkeydown = function(event) {
		event = event || window.event;
		var keyCode = event.which || event.keyCode;
		
		if(event.preventDefault) // Netscape/Firefox/Opera
			event.preventDefault();
		event.returnValue = false;

		debug.clear();

		

		debug.println(event.which+" "+event.keyCode+" "+keyCode);

		debug.print(self.getDimension()+" "+self.getPosition()+" >>> ");

		image.onload='null';


		var position = self.getPosition();
	
		switch(keyCode){
			
		// navigating frames
			// next frame
			case 33: // PageUp
			case 190: // . >
				self.nextFrame();
				break;
			// previous frame
			case 34: // PageDown
			case 188: // , <
				self.prevFrame();
				break;
			// last frame
			case 35: // End
			case 191: // / ?
				self.lastFrame();
				break;
			// first frame
			case 36: // Home
			case 77: // m
				self.firstFrame();
				break;		
		// zooming
			// zoom in
			case 88: // x
			case 43: // + / Numpad +
			case 61:
			case 107:
			case 187:
				self.zoomIn();
				break;
			// zoom out
			case 90: // z
			case 45: // - / Numpad -
			case 109: 
			case 173:
			case 189: 
				self.zoomOut();
				break;
			// zoom to 100%
			case 48: // 0
				self.originalSize();
				break
		// reset zoom
			case 82: // r
			case 32: // Space bar
		// center image
			case 67: // c
				self.fitImage();
				break;
		// moving 
			case 37: // Left arrow
				position[0] += currentMouseSpeed;
				break;
			case 38: // Up arrow
				position[1] += currentMouseSpeed;
				break;
			case 39: // Right arrow
				position[0] -= currentMouseSpeed;
				break;
			case 40: // Down arrow
				position[1] -= currentMouseSpeed;
				break;
		
		}

		if( keyCode>=37 && keyCode<=40){ // when moving
			position = self.centerImage(image.width,image.height, position[0],position[1]);
			self.setPosition(position[0],position[1]);
			currentMouseSpeed+=baseMouseSpeed;
		}

	debug.println(self.getDimension()+" "+self.getPosition());
	}
	self.onkeyup = function(event) {
		currentMouseSpeed = baseMouseSpeed;
	}
	/*Initializaion*/
	self.setZoom = function(newZoomLevel, newMinZoomLevel, newMaxZoomLevel) {
		if(newZoomLevel!=null)
			zoomLevel = newZoomLevel;
		if(newMinZoomLevel!=null)
			minZoomLevel = newMinZoomLevel;
		if(newMaxZoomLevel!=null)
			maxZoomLevel = newMaxZoomLevel;
	}
	self.setFrameProp = function(newFrameProp) {
		self.frameElement.style.width=newFrameProp[0];
		self.frameElement.style.height=newFrameProp[1];
	}
	self.setFrame = function(frame) {
		if(typeof frame=='undefined')
			frame = 1;
		if( frame<1 || frame>framesTotal )
			return false;

		// determine filename
		var framename = imageName;
		if(framesTotal>1){
			filename = imageName.split('.');
  			extension = filename.pop();
			framename = filename.join('.')+'-'+(frame-1)+'.'+extension;
		}
		image.src = framename;

		currentFrame = frame;
		self.updateStatus("Frame", currentFrame+'/'+framesTotal);
		
		// button locking
		buttons['first'].disable(currentFrame==1);
		buttons['prev'].disable(currentFrame==1);
		buttons['next'].disable(currentFrame==framesTotal);
		buttons['last'].disable(currentFrame==framesTotal);
		
		return true;
	}

	////////////////// BUTTONS
	createButton = function(name, value, title, onclick, group){
		var button = document.createElement('button');
		button.id = name;
		button.innerHTML = value;
		button.title = title;
		button.onclick = onclick;
		button.onmouseover = function(){this.style.color='#33CC00'};
		button.onmouseout =  function(){this.style.color=''};
		button.disable = function(disable){this.disabled=disable; this.style.color=''; this.blur();};
		buttons[name] = button;
		group.appendChild(button);
		return(button);
	}
	createStatusElement = function(name){
		var statusElement = document.createElement('div');
		statusElement.id = name;
		statusElement.className='status';
		status[name] = statusElement;
		statusbar.appendChild(statusElement);
		return(statusElement);
	}
	this.updateStatus = function(name, value){
		status[name].innerHTML = name+': '+value+'&nbsp;';
	}
	this.updatePosition = function(event){
		this.updateStatus("Position", '('+getMouseXY(event)+')');
	}
	///////////////////////////

	self.initImage = function() {
		image.style.maxWidth=image.style.width=image.style.maxHeight=image.style.height=null;

		
// 		var dimension=self.fitToFrame(originalW, originalH);
// 		self.setDimension(dimension[0],dimension[1]);
// 		
// 		var pos = self.centerImage(dimension[0],dimension[1], 0,0);
// 		self.setPosition(pos[0],pos[1]);
 		

		self.resetFrame();
		//self.setMouseCursor();

		//Set mouse handlers
		mouseWheelObject = new mouseWheel();
		mouseWheelObject.init(window, self.onmousewheel);
		image.onmousedown = self.onmousedown;
		image.onmousemove = self.onmousemove;
		image.onmouseout = self.onmouseout;
		//image.onmouseover = this.updatePosition;
		
		//Set keyboard handlers
 		self.frameElement.onkeydown = self.onkeydown;
		self.frameElement.onkeyup = self.onkeyup;
		onkeydown = self.onkeydown;
		onkeyup = self.onkeyup;


		//windowResize
		window.onresize = self.resetFrame;
		
		if(viewer.onload!=null)
			viewer.onload(self);
		if(self.onload!=null)
			self.onload();
	}
	self.preInitImage = function() { //Triggers after pre-Loader image has been loaded					
		if(preLoader!=null) 
		{
			image.style.left=((self.frameElement.clientWidth-image.width)/2) + 'px';
			image.style.top=((self.frameElement.clientHeight-image.height)/2) + 'px';
		}
		image.onload=self.initImage;
		image.src=imageSource;
	}				
	self.setNewImage = function(newImageSource,newPreLoader) {
		if(typeof newImageSource=='undefined')
			return;
		imageSource=newImageSource;
		if(typeof newPreLoader!=='undefined')
			preLoader=newPreLoader;
		if(preLoader!=null) {
			image.onload=self.preInitImage;
			image.src=preLoader;
			return;
		}
		image.onload=self.initImage;
		//image.src=imageSource;
		self.setFrame();
		
			
		this.updateStatus("Image", originalW+'x'+originalH);
	}

	
	/*Set a base*/
	self.setZoom();
	//Create self.frameElement - One time initialization
	self.frameElement=document.createElement('div');
	self.frameElement.style.display='block'
	self.frameElement.style.border="0px solid #000";
	self.frameElement.style.margin="0px";
	self.frameElement.style.padding="0px";
	self.frameElement.style.overflow="hidden";
	self.frameElement.style.position="relative";
	self.frameElement.style.zIndex=2;
	self.frameElement.tabIndex=1;
	self.frameElement.style.background="black";

			
	if(image!=null) {
		if (parent != null) {
			image.parentNode.removeChild(image);
			parent.appendChild(self.frameElement);
		}
		else if (replace != null) {
			image.parentNode.removeChild(image);
			replace.parentNode.replaceChild(self.frameElement, replace);
		}
		else
			image.parentNode.replaceChild(self.frameElement,image);
		
		image.style.margin=image.style.padding="0";
		image.style.borderWidth="0px";
		image.style.position='absolute';
		image.style.zIndex=3;
		self.frameElement.appendChild(image);
		
		if(imageSource!=null)
			self.preInitImage();
		else
			self.initImage();
	}
	else {		
		if(parent!=null){
			// create toolbar
			toolbar = document.createElement('div');
			toolbar.id = 'toolbar';
			parent.appendChild(toolbar);
			// create navbuttons container
			navbuttons = document.createElement('div');
			navbuttons.className='buttons';
			navbuttons.id='navbuttons';
			toolbar.appendChild(navbuttons);
			// create zoombuttons container
			zoombuttons = document.createElement('div');
			zoombuttons.className='buttons';
			zoombuttons.id='zoombuttons';
			toolbar.appendChild(zoombuttons);

			// create navigation buttons
			createButton('first','&#171;<br/>&nbsp;','First frame [HOME] [M] ',this.firstFrame,navbuttons);
			createButton('prev','&lt;','Previous frame [PAGE DOWN] [<]',this.prevFrame,navbuttons);
			createButton('next','&gt;','Next frame [PAGE UP] [>]',this.nextFrame,navbuttons);
			createButton('last','&#187;<br/>&nbsp;','Last frame [END] [?]',this.lastFrame,navbuttons);
	
			// create zoom buttons
			createButton('in','+','Zoom in [+] [X]',this.zoomIn,zoombuttons);
			createButton('out','&#8722;','Zoom out [-] [Z]',this.zoomOut,zoombuttons);
			createButton('org','1:1','Original size [0]',this.originalSize,zoombuttons).style.fontSize='14px';
			createButton('fit','&#8727;<br/>&nbsp;','Fit image [SPACEBAR] [C] [R]',this.fitImage,zoombuttons);
			
			
			
			
			// frame
			parent.appendChild(self.frameElement);

			//create status
			statusbar = document.createElement('div');
			statusbar.id = 'statusbar';
			parent.appendChild(statusbar);

			// create status elements
			createStatusElement("Image");
			createStatusElement("Frame");
			createStatusElement("Zoom");
			createStatusElement("Position");

		}
		else if(replace!=null)
			replace.parentNode.replaceChild(self.frameElement,replace);
			
		image=document.createElement('img');
		image.style.position='absolute';
		image.style.zIndex=3;
		self.frameElement.appendChild(image);
		
		self.setNewImage(imageSource);
	}
	//Experimental
	if(borderClass!=null) { //Browser rendering of borders with padding have been a problem.
		self.outerFrame = document.createElement('div');
		self.outerFrame.className=borderClass;
		self.frameElement.parentNode.replaceChild(self.outerFrame,self.frameElement);
		self.outerFrame.appendChild(self.frameElement);
	}



}
//Static events
viewer.onload = null;


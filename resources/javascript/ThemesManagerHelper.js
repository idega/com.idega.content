var THEME_ID = null;
var THEMES = new Array();
var THEME_CHANGES = new Array();

var imageWidth = 149;
var imageHeight = 112;
var CURRENT_THEME_INDEX = 0;
var waitForStyle = 100;
var SLIDE_DURATION = 500;
var SHOW_SLIDER_BUTTONS_ID = 0;
var THEMES_REFLECTION_ID = 0;

var CONTAINER_ID = 'themes';
var THEMES_CONTAINER_ID = 'themesSliderContainer';
var SHOW_THEMES_BUTTON_ID = 'showThemesButton';

var ENABLE_STYLE_FUNCTIONS = true;
var ENABLE_THEME_CONTAINER = true;
var ENABLE_STYLE_VARIATIONS = true;

var needReflection = false;
var needApplyThemeForSite = false;

function getImageWidth() {
	return imageWidth;
}

function getAllImageSpace() {
	return (getImageWidth() + 8);
}

function getImageHeight() {
	return imageHeight;
}

function isCorrectFileType(id, fileType, noFileMsg, invalidFileTypeMsg) {
	var input = $(id);
	if (input) {
		if (input.value == '') {
			alert(noFileMsg);
			return false;
		}
		var nameParts = input.value.split('.');
		if (nameParts) {
			var lastPart = nameParts[nameParts.length - 1];
			if (lastPart.toLowerCase() != fileType) {
				alert(invalidFileTypeMsg);
				return false;
			}
		}
	}
	return true;
}

function getThemeStyleVariations(themeID) {
	if (THEME_ID != null) {
		var oldTheme = getTheme(THEME_ID);
		if (oldTheme != null) {
			var oldClassName = 'themeName';
			if (oldTheme.used) {
				oldClassName = 'themeName usedThemeName';
			}
			setNewStyleForSelectedElement(oldTheme.id + '_themeNameContainer', oldClassName);
		}
	}
	showLoadingMessage(getLoadingText());
	setGlobalId(themeID);
	var newTheme = getTheme(THEME_ID);
	if (!newTheme.used) {
		setNewStyleForSelectedElement(newTheme.id + '_themeNameContainer', 'themeName selectedThemeName');
	}
	setThemeForPreview(themeID);
	ThemesEngine.getThemeStyleVariations(themeID, getThemeStyleVariationsCallback);
}

function getThemeStyleVariationsCallback(variations) {
	closeLoadingMessage();
	if (variations == null) {
		return;
	}
	var theme = getTheme(THEME_ID);
	if (theme != null) {
		setIfUsedTheme(theme.used);
	}
	else {
		setIfUsedTheme(false);
	}
	var container = $('themeStyleVariations');
	if (container == null) {
		return;
	}
	removeChildren(container);
	container = replaceHtml(container, variations);
	//container.innerHTML = variations;
	
	registerThemesActions();
}

function changeTheme(themeID, styleGroupName, newStyleMember, type, checked) {
	var radio = true;
	if (type == 'checkbox') {
		radio = false;
	}
	var themeNameObj = $('theme_name');
	if (themeNameObj != null) {
		showLoadingMessage(getThemeChangingText());
		ThemesEngine.changeTheme(themeID, styleGroupName, newStyleMember, themeNameObj.value, radio, checked, changeThemeCallback);
	}
}

function changeThemeCallback(themeID) {
	THEME_CHANGES = new Array();
	closeLoadingMessage();
	if (themeID == null) {
		$('themeSaveButton').disabled = true;
	}
	else {
		setGlobalId(themeID);
		$('themeSaveButton').disabled = false;
		getThemes(themeID, true, false);
	}
}

function saveTheme() {
	if (THEME_ID != null) {
		showLoadingMessage(getThemeSavingText());
		var themeNameObj = $('theme_name');
		if (themeNameObj != null) {
			ThemesEngine.saveTheme(THEME_ID, themeNameObj.value, saveThemeCallback);
		}
	}
}

function saveThemeCallback(result) {
	closeLoadingMessage();
	if (result) {
		$('themeSaveButton').disabled = false;
		getThemes(THEME_ID, true, false);
	}
	else {
		if (needApplyThemeForSite) {
			needApplyThemeForSite = false;
			applyThemeForSite(THEME_ID);
		}
	}
}

function manageButton(id, disable) {
	var button = $(id);
	if (button != null) {
		button.disabled = disable;
	}
}

function setGlobalId(themeId) {
	THEME_ID = themeId;
}

function setThemeName(themeName) {
	removeStyleOptions();
	var themeNameObj = $('theme_name');
	if (themeNameObj != null) {
		themeNameObj.value = themeName;
	}
}

function getThemesContainerLength() {
	var container = $(CONTAINER_ID);
	if (container == null) {
		return 0;
	}
	var containerWidth = 0;
	var containerWidthWithPixels = $(CONTAINER_ID).style.width;
	var temp = containerWidthWithPixels.split('px');
	if (temp != null) {
		var widthValue = temp[0];
		widthValue++;
		widthValue--;
		containerWidth = widthValue;
	}
	return containerWidth;
}

function getThemesContainerLeftValue() {
	var themesLength = 0;
	var containerLeftValue = 0;
	if ($(CONTAINER_ID).style.left) {
		var pixelsFromLeft = $(CONTAINER_ID).style.left;
		temp = pixelsFromLeft.split('px');
		if (temp != null) {
			containerLeftValue = temp[0];
			containerLeftValue++;
			containerLeftValue--;
			themesLength = containerLeftValue;
		}
	}
	
	return themesLength;
}

function getThemesTickerContainerLength() {
	var tickerFrom = getAbsoluteLeft('themesTickerContainer');
	var tickerTo = getAbsoluteLeft('rightScrollerContainer');
	return (tickerTo - tickerFrom);
}

function scroll(id) {
	removeStyleOptions();
	if (id == null) {
		return;
	}
	
	var themesToSlide = 1;
	
	// Getting ticker length
	var tickerWidth = getThemesTickerContainerLength();
	
	// Getting themes container length
	var containerWidth = getThemesContainerLength();
	
	// Checking if need to scroll
	if (tickerWidth > containerWidth) {	// All themes are visible in slider
		moveSlider(0);	// Restoring slider		return;
	}
	themesToSlide = Math.floor(tickerWidth / getAllImageSpace());
	if (themesToSlide < 0) {	// No space to slide?
		return;
	}
	
	var direction = 'right';
	if (id == 'leftScroller') {
		direction = 'left';
	}
	
	scrollThemes(direction, themesToSlide, true);
}

function scrollThemes(direction, themesToSlide, useMooTools) {
	if (direction == null || themesToSlide == null) {
		return;
	}
	
	var move = true;
	
	var moveToPosition = getThemesContainerLeftValue();
	
	if (direction == 'left') {
		// Move to the left
		if ((CURRENT_THEME_INDEX - themesToSlide) >= 0) {
			CURRENT_THEME_INDEX -= themesToSlide;
			moveToPosition += (getAllImageSpace() * themesToSlide);
		}
		else {
			move = false;
		}
	}
	else {
		if (direction == 'right') {
			// Move to the right
			if ((CURRENT_THEME_INDEX + themesToSlide) < THEMES.length) {
				CURRENT_THEME_INDEX += themesToSlide;
				moveToPosition -= (getAllImageSpace() * themesToSlide);
			}
			else {
				move = false;
			}
		}
	}
		
	if (move) {
		moveSlider(moveToPosition, useMooTools);
	}
}

function moveSlider(moveToPosition, useMooTools) {
	var themesContainer = $(CONTAINER_ID);
	if (themesContainer == null) {
		return;
	}
	if (useMooTools) {
		hideThemeSliderButtons();
		SHOW_SLIDER_BUTTONS_ID = window.setTimeout('showThemeSliderButtons()', SLIDE_DURATION);
		var slideMove = new Fx.Style(themesContainer, 'left', {duration: SLIDE_DURATION, transition: Fx.Transitions.quadOut});
		slideMove.start(moveToPosition);
	}
	else {
		themesContainer.style.left = moveToPosition + 'px';
	}
}

function showThemeSliderButtons() {
	var ids = new Array();
	ids.push('leftScrollerContainer');
	ids.push('rightScrollerContainer');
	setElementsVisibilityProperty(ids, 'visible');
	window.clearTimeout(SHOW_SLIDER_BUTTONS_ID);
}

function hideThemeSliderButtons() {
	var ids = new Array();
	ids.push('leftScrollerContainer');
	ids.push('rightScrollerContainer');
	setElementsVisibilityProperty(ids, 'hidden');
}

function setElementsVisibilityProperty(ids, property) {
	if (ids == null) {
		return;
	}
	var element = null;
	for (var i = 0; i < ids.length; i++) {
		element = $(ids[i]);
		if (element != null) {
			element.style.visibility = property;
		}
	}
}

function getThemes(themeID, addReflect, needScrollToDefaultTheme) {
	showLoadingMessage(getGeneratingPreviewText());
	setGlobalId(themeID);
	needReflection = addReflect;
	ThemesEngine.getThemes({
		callback: function(themes) {
			getThemesCallback(themes, needScrollToDefaultTheme);
		}
	});
}

function getThemesCallback(themes, needScrollToDefaultTheme) {
	closeLoadingMessage();
	if (themes == null) {
		hideThemesContainer();
		return;
	}

	showThemesContainer();
	var container = $(CONTAINER_ID);
	if (container == null) {
		return;
	}
	
	removeChildren(container);
	
	THEMES = new Array();
	var simpleTheme = null;
	var date = new Date();
	var time = null;
	var theme = null;
	
	for (var i = 0; i < themes.length; i++) {
		simpleTheme = themes[i];
		
		if (simpleTheme.linkToSmallPreview != null && simpleTheme.linkToSmallPreview != '') {
			time = date.getTime();
			theme = new Theme(simpleTheme.name, simpleTheme.linkToSmallPreview + '?' + time, simpleTheme.linkToBigPreview + '?' + time, simpleTheme.id, simpleTheme.used);
			
			var div = new Element('div');
			div.setAttribute('id', theme.id + '_mainContainer');
			div.className = 'imageGallery';
			
			// Is used?
			if (theme.used) {
				div.setAttribute('title', $('defaultThemeLabel').value);
			}
			else {
				div.setAttribute('title', $('notDefaultThemeLabel').value);
			}
	
			// Name
			var textDiv = new Element('div');
			textDiv.setAttribute('id', theme.id + '_themeNameContainer');
			if (theme.used) {
				textDiv.className = 'themeName usedThemeName';
			}
			else {
				textDiv.className = 'themeName';
			}
			var themeNameContainer = new Element('strong');
			themeNameContainer.appendChild(document.createTextNode(theme.name));
			textDiv.appendChild(themeNameContainer);
			div.appendChild(textDiv);
			
			var imageDiv = new Element('div');
			imageDiv.setAttribute('id', theme.id + '_container');
			var image = new Element('img'); 
	   		image.setAttribute('id', theme.id); 
	   		image.setAttribute('src', theme.linkToSmallPreview);
	   		image.setAttribute('width', getImageWidth() + 'px');
	   		image.setAttribute('height', getImageHeight() + 'px');
	   		//image.setAttribute('title', theme.themeName);
	   		
	        image.className = 'reflect rheight18 ropacity68';
	        imageDiv.className = 'galleryImage firstInRow';
	        imageDiv.appendChild(image);
	        div.appendChild(imageDiv);
	        
			container.appendChild(div);
	
			if (ENABLE_STYLE_VARIATIONS) {
	   			image.addEvent('click', function() {
	   				getThemeStyleVariations(this.id);
	   			});
	   		}
	   		if (ENABLE_STYLE_FUNCTIONS) {
	   			image.addEvents({
	   				'mouseover': function() {
		   				chooseStyle(this.id);
		   			},
		   			'mouseout': function() {
		   				recallStyle(this.id);
		   			}
	   			});
	   		}
			
			THEMES.push(theme);
		}
	}
	container.style.width = Math.round(THEMES.length * getAllImageSpace()) + 'px';
	
	// Setting default theme
	if (needScrollToDefaultTheme) {
		theme = getDefaultTheme();
		if (theme != null) {
			scrollToDefaultTheme();
		}
	}
	else {
		if (THEME_ID != null) {
			theme = getTheme(THEME_ID);	// Getting last used theme
		}
	}
	
	// No default or last used theme - getting the first from list
	if (theme == null) {
		theme = THEMES[0];
	}
	
	if (needApplyThemeForSite) {
		needApplyThemeForSite = false;
		applyThemeForSite(THEME_ID);
	}
	
	if (theme != null) {
		setGlobalId(theme.id);
		if (ENABLE_STYLE_VARIATIONS) {
			getThemeStyleVariations(theme.id);
		}	
	}
	
	// Adding reflection
	addReflectionToThemes();
}

function getDefaultTheme() {
	var theme = null;
	for (var i = 0; i < THEMES.length; i++) {
		theme = THEMES[i];
		if (theme.used) {
			return theme;
		}
	}
	return null;
}

function scrollToDefaultTheme() {
	if (THEMES == null) {
		return;
	}
	
	var theme = null;
	var index = 0;
	var foundDefault = false;
	for (index = 0; (index < THEMES.length && !foundDefault); index++) {
		theme = THEMES[index];
		if (theme.used) {
			foundDefault = true;
		}
	}
	if (!foundDefault) {
		return;
	}
	
	if (isVisibleDefaultTheme(index)) {
		return;
	}
	
	var availableWidth = getThemesTickerContainerLength();
	var visibleThemes = Math.floor(availableWidth / getAllImageSpace());
	
	var realIndex = index - 1;
	var timesToSlide = Math.floor(realIndex / visibleThemes);
	for (var i = 0; i < timesToSlide; i++) {
		scrollThemes('right', visibleThemes, false);
	}
}

function isVisibleDefaultTheme(index) {
	if (index == null) {
		return true;
	}
	
	var neededWidthForDefaultTheme = index * getAllImageSpace();
	var availableWidth = getThemesTickerContainerLength();
	if (availableWidth <= 0) {
		return true;
	}
	var visibleThemes = Math.floor(availableWidth / getAllImageSpace());
	if (visibleThemes >= index) {
		return true;
	}
	return false;
}

function setIfUsedTheme(used) {
	var element = $('themeUsability');
	if (element == null) {
		return;
	}
	if (element.childNodes != null) {
		for (var i = 0; element.childNodes.length; i++) {
			element.removeChild(element.childNodes[i]);
		}
	}
	var text = '';
	if (used) {
		text = $('defaultThemeLabel').value;
	}
	else {
		text = $('notDefaultThemeLabel').value;
	}
	element.appendChild(document.createTextNode(text));
	
	highlightElement(element, 1500, '#F2F2F2');
}

function Theme(name, linkToSmallPreview, linkToBigPreview, id, used) {
	this.name = name;
	this.linkToSmallPreview = linkToSmallPreview;
	this.linkToBigPreview = linkToBigPreview;
	this.id = id;
	this.applyStyle = false;
	this.used = used;
}

function setPreview(url) {
	var preview = $('themePreview');
	if (preview == null) {
		return;
	}
	preview.src = url;
}

function getTheme(themeID) {
	var theme = null;
	for (var i = 0; i < THEMES.length; i++) {
		theme = THEMES[i];
		if (theme.id == themeID) {
			return theme;
		}
	}
	return null;
}

function setThemeForPreview(themeID) {
	var theme = getTheme(themeID);
	if (theme != null) {
		setPreview(theme.linkToBigPreview);
		setThemeName(theme.name);
	}
}

function getThemeIndex(themeID) {
	var theme = null;
	for (var i = 0; i < THEMES.length; i++) {
		theme = THEMES[i];
		if (theme.id == themeID) {
			return i;
		}
	}
	return -1;
}

function removeStyleOptions() {
	var div = $('chooseStyleLayer');
	if (div != null) {
		div.style.display = 'none';
	}
}

function initScript(useStyling, enableContainer, enableVariations) {
	ENABLE_STYLE_FUNCTIONS = useStyling;
	ENABLE_THEME_CONTAINER = enableContainer;
	ENABLE_STYLE_VARIATIONS = enableVariations;
}

function hideThemesContainer() {
	var container = $(THEMES_CONTAINER_ID);
	if (container != null) {
		container.style.display = 'none';
	}
	manageButton(SHOW_THEMES_BUTTON_ID, true);
}

function showThemesContainer() {
	if (ENABLE_THEME_CONTAINER) {
		var container = $(THEMES_CONTAINER_ID);
		if (container != null) {
			container.style.display = 'block';
		}
	}
	manageButton(SHOW_THEMES_BUTTON_ID, false);
}

function restoreTheme() {
	showLoadingMessage(getRestoringThemeText());
	ThemesEngine.restoreTheme(THEME_ID, restoreThemeCallback);
}

function restoreThemeCallback(result) {
	closeLoadingMessage();
	getThemes(THEME_ID, true, false);
}

function addReflectionToThemes() {
	if (needReflection) {
		for (var i = 0; i < THEMES.length; i++) {
			Reflection.add(THEMES[i].id, { height: 18/100, opacity: 68/100 });
		}
	}
}

function saveAndApplyTheme() {
	if (THEME_ID == null) {
		return false;
	}
	needApplyThemeForSite = true;
	saveTheme();
}

function changeVariations() {
	if (THEME_ID == null) {
		return false;
	}
	if (THEME_CHANGES == null) {
		return false;
	}
	if (THEME_CHANGES.length == 0) {
		return false;
	}
	var themeNameObj = $('theme_name');
	if (themeNameObj != null) {
		showLoadingMessage(getThemeChangingText());
		ThemesEngine.applyMultipleChangesToTheme(THEME_ID, THEME_CHANGES, themeNameObj.value, changeThemeCallback);
	}
}

function ThemeChange(themeId, styleGroupName, variation, radio, enabled) {
	this.themeId = themeId;
	this.styleGroupName = styleGroupName;
	this.variation = variation;
	this.radio = radio;
	this.enabled = enabled;
}

function removeThemeChange(index, elementsToRemove) {
	if (THEME_CHANGES == null) {
		return false;
	}
	if (index >= 0 && index < THEME_CHANGES.length) {
		THEME_CHANGES.splice(index, elementsToRemove);
		return true;
	}
	return false;
}

function addThemeChange(themeId, styleGroupName, variation, variationType, enabled) {
	if (THEME_ID != themeId) {
		THEME_CHANGES = new Array();	// Reseting array of changes
	}
	var radio = true;
	if (variationType == 'checkbox') {
		radio = false;
		removeThemeChange(existThemeChange(themeId, styleGroupName, variation, radio, enabled), 1);
	}
	if (radio) {
		removeSameGroupChanges(themeId, styleGroupName);
	}
	
	var index = existThemeChange(themeId, styleGroupName, variation, radio, enabled);
	if (index < 0) {
		THEME_CHANGES.push(new ThemeChange(themeId, styleGroupName, variation, radio, enabled));
	}
}

function removeSameGroupChanges(themeId, styleGroupName) {
	if (THEME_CHANGES == null) {
		return false;
	}
	var themeChange = null;
	var elementsToRemove = new Array();
	for (var i = 0; i < THEME_CHANGES.length; i++) {
		themeChange = THEME_CHANGES[i];
		if (themeChange.themeId == themeId && themeChange.styleGroupName == styleGroupName) {
			elementsToRemove.push(i);
		}
	}
	for (var i = 0; i < elementsToRemove.length; i++) {
		removeThemeChange(elementsToRemove[i], 1);
	}
}

function existThemeChange(themeId, styleGroupName, variation, radio, enabled) {
	if (THEME_CHANGES == null) {
		return -1;
	}
	var existChange = false;
	var themeChange = null;
	var i = 0;
	for (i = 0; (i < THEME_CHANGES.length && !existChange); i++) {
		themeChange = THEME_CHANGES[i];
		if (themeChange.themeId == themeId && themeChange.styleGroupName == styleGroupName && themeChange.variation == variation) {
			existChange = true;
		}
	}
	if (existChange) {
		if (i == 0) {
			return 0;
		}
		return i - 1;
	}
	return -1;
}

function initializeThemes() {
	insertStyleFile();
	initScript(false, true, true);
	resizeThemesContentContainer();
	getThemes(null, true, true);
}

function resizeThemesContentContainer() {
	var container = $('themePreviewAndSliderContainer');
	if (container == null) {
		return;
	}
	
	//	Width
	var reservedWidth = 490;
	var totalWidth = getTotalWidth();
	if (totalWidth > reservedWidth) {
		container.style.width = (totalWidth - reservedWidth) + 'px';
	}
	
	var themePreviewContainer = $('themePreviewContainer');
	if (themePreviewContainer == null) {
		return;
	}
	themePreviewContainer.style.width = (totalWidth - reservedWidth - 10) + 'px';
	
	var themesTickerContainer = $('themesTickerContainer');
	if (themesTickerContainer == null) {
		return;
	}
	themesTickerContainer.style.width = (totalWidth - reservedWidth - 50) + 'px';
	
	//	Height
	var mainThemesContainer = $('mainThemesContentContainer');
	if (mainThemesContainer == null) {
		return;
	}
	var reservedHeight = 75;
	var totalHeight = getTotalHeight();
	if (totalHeight > reservedHeight) {
		mainThemesContainer.style.height = (totalHeight - reservedHeight) + 'px';
	}
	
	var themeVariationsContainer = $('themeVariationsContainer');
	if (themeVariationsContainer == null) {
		return;
	}
	themeVariationsContainer.style.height = (totalHeight - reservedHeight - 5) + 'px';
	themePreviewContainer.style.height = (totalHeight - reservedHeight - 165) + 'px';
}

function registerThemesActions() {
	$$('img.reload_properties_for_theme').each(
		function(element) {
			initToolTipForElement(element);
			element.onclick = function() {
				reloadThemeProperties(element);
			}
		}
	);
}

function reloadThemeProperties(element) {
	if (element == null) {
		return null;
	}
	var attr = element.attributes;	
	var themeId = null;
	if (attr.getNamedItem('current_theme_id') != null) {
		themeId = attr.getNamedItem('current_theme_id').value;
	}
	
	showLoadingMessage(getReloadingText());
	ThemesEngine.reloadThemeProperties(themeId, getThemeStyleVariationsCallback);
}
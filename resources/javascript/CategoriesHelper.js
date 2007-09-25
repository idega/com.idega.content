var LOADING_TEXT = 'Loading...';
var DELETING_TEXT = 'Deleting...';
var SAVING_TEXT = 'Saving...';
var DISABLE_CATEGORY_TEXT = 'Do You want to disable this category?';
var ENABLE_CATEGORY_TEXT = 'Do You want to enable this category?';
var DELETE_CATEGORY_TEXT = 'Do You want to delete this category?';
var DISABLE_CATEGORY_IMAGE_TEXT = 'Disable category';
var ENABLE_CATEGORY_IMAGE_TEXT = 'Enable category';
var ENTER_CATEGORY_NAME = 'Please, enter name for category!';
var NO_CATEGORIES_TEXT = 'There are no categories.';
var EDIT_CATEGORY_WINDOW_TITLE = 'Edit category';

var EDIT_CATEGORY_WINDOW_CLASS = 'com.idega.content.presentation.categories.CategoryEditor';

function getInfoForCategories() {
	CategoriesEngine.getInfo({callback: function(info) {	
		if (info == null) {
			return false;
		}
		
		LOADING_TEXT = info[0];
		DISABLE_CATEGORY_TEXT = info[1];
		ENABLE_CATEGORY_TEXT = info[2];
		DELETE_CATEGORY_TEXT = info[3];
		DISABLE_CATEGORY_IMAGE_TEXT = info[4];
		ENABLE_CATEGORY_IMAGE_TEXT = info[5];
		DELETING_TEXT = info[6];
		ENTER_CATEGORY_NAME = info[7];
		SAVING_TEXT = info[8];
		NO_CATEGORIES_TEXT = info[9];
		EDIT_CATEGORY_WINDOW_TITLE = info[10];
		EDIT_CATEGORY_WINDOW_CLASS = info[11];
		
	}});
}

function initializeContentCategoriesActions() {
	$$('select.localesForCategoriesDropDownMenuStyle').each(
		function(select) {
			select.addEvent('change', function() {
				reloadCategoriesByLocale(select.value, select.getProperty('containerid'));
			});
		}
	);
	
	$$('input.addCategoryButtonStyle').each(
		function(button) {
			button.addEvent('click', function() {
				addNewCategory(button.getProperty('categorynameid'), button.getProperty('localesid'), button.getProperty('categorieslistid'));
			});
		}
	);

	$$('input.nameForNewCategoryInputStyle').each(
		function(input) {
			input.addEvent('keyup', function(e) {
				var event = new Event(e);
				if (event.key == 'enter') {
					addNewCategory(input.getProperty('categorynameid'), input.getProperty('localesid'), input.getProperty('categorieslistid'));
					return false;
				}
				return false;
			});
			input.addEvent('blur', function() {
				addNewCategory(input.getProperty('categorynameid'), input.getProperty('localesid'), input.getProperty('categorieslistid'));
			});
		}
	);
	
	initializeCategoryManagementActions();
}

function initializeCategoryManagementActions() {
	$$('img.changeCategoryUsageImageStyle').each(
		function(image) {
			image.addEvent('click', function() {
				changeCategoryStatus(image);
			});
		}
	);
	
	$$('img.deleteCategoryImageStyle').each(
		function(image) {
			image.addEvent('click', function() {
				deleteCategory(image);
			});
		}
	);
	
	var categories = $$('span.changeCategoryNameLabelStyle');
	$$('span.changeCategoryNameLabelStyle').each(
		function(category) {
			category.addEvent('click', function() {
				changeCategoryName(category);
			});
		}
	);
}

function changeCategoryName(category) {
	var link = '/servlet/ObjectInstanciator?idegaweb_instance_class=' + EDIT_CATEGORY_WINDOW_CLASS + '&categoryId=' + category.getProperty('categoryid') +
				'&categoryLocale=' + category.getProperty('language');
	MOOdalBox.init({resizeDuration: 50, evalScripts: true, animateCaption: false});
	var result = MOOdalBox.open(link, EDIT_CATEGORY_WINDOW_TITLE, '350 200');
	return false;
}

function addNewCategory(inputId, selectId, categoriesListId) {
	if (inputId == null || selectId == null || categoriesListId == null) {
		return false;
	}
	
	var input = $(inputId);
	var select = $(selectId);
	if (input == null || select == null) {
		return false;
	}
	
	var categoryName = input.value;
	if (categoryName == null || categoryName == '') {
		alert(ENTER_CATEGORY_NAME);
		return false;
	}
	
	var language = 'en';
	if (select.options != null && select.options.length > 0) {
		language = select.options[select.selectedIndex].value;
	}
	
	showLoadingMessage(SAVING_TEXT);
	CategoriesEngine.addCategory(categoryName, language, {
		callback: function(categoriesList) {
			input.setProperty('value', '');
			closeAllLoadingMessages();
			if (categoriesList == null) {
				return false;
			}
			
			var container = $(categoriesListId);
			if (container == null) {
				return false;
			}
			
			removeChildren(container);
 			insertNodesToContainer(categoriesList, container);
 			initializeCategoryManagementActions();
		}
	});
}

function initializeCategoryEditorWindowActions() {
	$$('input.changeCategoryNameButtonStyle').each(
		function(button) {
			button.addEvent('click', function() {
				renameCategory(button.getProperty('categoryid'), $(button.getProperty('newnameinputid')).value, button.getProperty('language'));
			});
		}
	);
	
	$$('input.changeCategoryNameInputStyle').each(
		function(input) {
			input.addEvent('keyup', function(e) {
				var event = new Event(e);
				if (event.key == 'enter') {
					renameCategory(input.getProperty('categoryid'), $(input.getProperty('newnameinputid')).value, input.getProperty('language'),
						input.getProperty('reloadcategories'), input.getProperty('removecontainerid'));
					return false;
				}
				return false;
			});
			input.addEvent('blur', function() {
				renameCategory(input.getProperty('categoryid'), $(input.getProperty('newnameinputid')).value, input.getProperty('language'),
					input.getProperty('reloadcategories'), input.getProperty('removecontainerid'));
			});
		}
	);
}

function renameCategory(id, newName, language, needReload, removeContainerId) {
	if (id == null || language == null) {
		return false;
	}
		
	var reload = true;
	if (needReload == null) {
		reload = false;
	}
	else {
		if (needReload == 'false') {
			reload = false;
		}
	}
	
	if (newName == null || newName == '') {
		alert(ENTER_CATEGORY_NAME);
		return false;
	}
	
	showLoadingMessage(SAVING_TEXT);
	CategoriesEngine.renameCategory(id, language, newName, {
		callback: function(result) {
			closeAllLoadingMessages();
			if (!result) {
				return false;
			}
			
			if (reload) {
				var containers = $$('div.categoriesByLocaleContainerStyle');
				if (containers == null) {
					return false;
				}
				if (containers.length == 0) {
					return false;
				}
				
				var id = containers[0].getProperty('id');
				if (id == null || id == '') {
					return false;
				}
				reloadCategoriesByLocale(language, id);
			}
			
			if (removeContainerId != null) {
				var parentContainer = $(removeContainerId).getParent();
				
				$(removeContainerId).remove();
				
				var localizedItems = $$('div.localizedCategoryContainerStyle');
				if (localizedItems != null && localizedItems.length == 0) {
					parentContainer.remove();
				}
				
			}
			return false;
		}
	});
}

function deleteCategory(image) {
	var confirmed = window.confirm(DELETE_CATEGORY_TEXT);
	if (confirmed) {
		showLoadingMessage(DELETING_TEXT);
		CategoriesEngine.deleteCategory(image.getProperty('categoryid'), {
			callback: function(result) {
				closeAllLoadingMessages();
				if (!result) {
					return false;
				}
				
				var categoriesContainer = image.getParent().getParent();
				image.getParent().remove();
				
				var addEmptyText = false;
				var categories = $$('div.categoryContainer');
				if (categories == null) {
					addEmptyText = true;
				}
				if (categories.length == 0) {
					addEmptyText = true;
				}
				if (addEmptyText) {
					var span = new Element('span');
					span.addClass('categoriesFontStyle');
					span.appendText(NO_CATEGORIES_TEXT);
					span.injectInside(categoriesContainer);
				}
			}
		});
	}
	else {
		return false;
	}
}

function changeCategoryStatus(image) {
	var isEnabled = image.getProperty('isenabled') == 'true';
	var text = ENABLE_CATEGORY_TEXT;
	if (isEnabled) {
		text = DISABLE_CATEGORY_TEXT;
	}
	var confirmed = window.confirm(text);
	if (confirmed) {
		showLoadingMessage(LOADING_TEXT);
		CategoriesEngine.manageCategoryUsage(image.getProperty('categoryid'), isEnabled, {
			callback: function(uri) {
				closeAllLoadingMessages();
				if (uri != null) {
					image.setProperty('src', uri);
					image.setProperty('isenabled', !isEnabled);
					var imageText = DISABLE_CATEGORY_IMAGE_TEXT;
					if (isEnabled) {
						imageText = ENABLE_CATEGORY_IMAGE_TEXT;
					}
					image.setProperty('title', imageText);
					image.setProperty('name', imageText);
				}
			}
		});
	}
	else {
		return false;
	}
}

function reloadCategoriesByLocale(locale, id) {
	showLoadingMessage(LOADING_TEXT);
	CategoriesEngine.getCategoriesList(locale, {
		callback: function(component) {
			getCategoriesListCallback(component, id);
		}
	});
}

 function getCategoriesListCallback(component, id) {
 	closeAllLoadingMessages();
 	if (component == null) {
 		return false;
 	}
 	
 	var container = $(id);
 	if (container == null) {
 		return false;
 	}
 	
 	removeChildren(container);
 	insertNodesToContainer(component, container);
 	initializeCategoryManagementActions();
 }
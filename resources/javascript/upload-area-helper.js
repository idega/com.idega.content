(function($) {
	   $.fn.uploadAreaHelper = function(options) {
		   
		   var opts = $.extend({}, $.fn.uploadAreaHelper.defaults, options);
		   
		   if((options.dropZone != null) && (options.dropZone != undefined)){
			   var dropZones = jQuery(eval(opts.dropZone));
			   opts.dropZone = dropZones;
			   var doc = jQuery(document);
			   dropZones.css({
				   'border' : 'solid',
				   'border-width' : '2px',
				   'border-color' : 'transparent',
				   'border-radius': '20px'
			   });
			   doc.bind('dragover',{dropZones : dropZones},function(e){
				   e.data.dropZones.css({
					   'border-color' : '#3333EE'
				   });
			   });
			   doc.bind('dragleave drop',{dropZones : dropZones},function(e){
				   e.data.dropZones.css({
					   'border-color' : 'transparent'
				   });
			   });
			// Disabling default action of droping files
				jQuery(document).bind('drop dragover', function (e) {
				    e.preventDefault();
				});
		   }
		   if((options.filesContainer != null) && (options.filesContainer != undefined)){
			   var filesContainerLayer = jQuery(eval(opts.filesContainer));
			   filesContainerLayer.append(options.uploadAreaOptions.filesContainerContent);
			   opts.filesContainer = filesContainerLayer.find(".files");
		   }
		   return this.each(function(){
			   var uploader = jQuery(this);

				opts.uploadAreaHelper = {};
				opts.uploadAreaHelper.element = uploader;
				uploader.fileupload( opts);
				
		   } ); 
		   
	   
	   }
	   
	   $.fn.uploadAreaHelper.defaults = {
		      	url : '/servlet/blueimp-upload',
		          maxFileSize: 5000000,
//		          acceptFileTypes: /(\.|\/)(gif|jpe?g|png)$/i,
		          maxFileSize: 1024*1024*1024,
		          process: [
		              {
		                  action: 'load',
		                  fileTypes: /^image\/(gif|jpeg|png)$/,
		                  maxFileSize: 20000000 // 20MB
		              },
		              {
		                  action: 'resize',
		                  maxWidth: 1440,
		                  maxHeight: 900
		              },
		              {
		                  action: 'save'
		              }
		          ]
		          ,uploadTemplateId: null
		          ,downloadTemplateId: null
		          ,uploadTemplate: function (o) {
		              var rows = $();
		              $.each(o.files, function (i, file) {
		                  var row = $('<tr class="template-upload fade"/>');
		                  var preview = $('<td class="preview"><span class="fade"></span></td>');
		                  row.append(preview);
		                  var name = $('<td class="name"></td>');
		                  row.append(name);
		                  name.text(file.name);
		                  var size = $('<td class="preview"><span class="fade"></span></td>');
		                  row.append(size);
		                  size.text(o.formatFileSize(file.size));
		                  if(file.error){
		                  	var errorMsg = $('<td class="error" colspan="2"><span class="label label-important"/>');
		                  	row.append(errorMsg);
		                  	errorMsg.text(locale.fileupload.errors[file.error] || file.error);
		                  	errorMsg.find('.label').text(locale.fileupload.error);
		                  }else if (o.files.valid && !i) {
		                  	var progress = $('<td class="upload-area-progressbar" ><div style="width:100%" class="progress progress-success progress-striped active" role="progressbar" ' + 
		                  			'aria-valuemin="0" aria-valuemax="100" aria-valuenow="0"><div class="bar" style="width:0%;"></div></div></td>');
		                  	row.append(progress);
		                  	var start = $('<td class="start" />');
		                  	row.append(start);
		                  	if(!o.options.autoUpload){
		                  		var button = $('<button class="btn btn-primary" />');
		                  		start.append(button);
		                  		var buttonContent = $('<i class="icon-upload icon-white"/><span>'+locale.fileupload.start+'</span>');
		                  		button.append(buttonContent);
		                  	}
		                  }else{
		                  	row.append('<td colspan="2"></td>');
		                  }
		                  var cancel = $('<td class="cancel" />');
		                  row.append(cancel);
		                  if(!i){
		                  	cancel.append('<button class="btn btn-warning"><i class="icon-ban-circle icon-white"></i><span>'+
		                  			locale.fileupload.cancel + '</span></button>');
		                  }
		                  rows = rows.add(row);
		              });
		              return rows;
		          }
		          ,downloadTemplate: function (o) {
		              var rows = $();
		              $.each(o.files, function (index, file) {
		                  var row = $('<tr class="template-download fade">' +
		                      (file.error ? '<td></td><td class="name"></td>' +
		                          '<td class="size"></td><td class="error" colspan="2"></td>' :
		                              '<td class="preview"></td>' +
		                                  '<td class="name"><a></a></td>' +
		                                  '<td class="size"></td><td colspan="2"></td>'
		                      ) + '<td class="delete"><button class="btn btn-danger" ><i class="icon-trash icon-white"></i><span>'+
		                      	locale.fileupload.destroy + '</span></button> '
		                          + (o.options.uploadAreaOptions.checkboxNeeded ? '<input type="checkbox" name="delete" value="1">' : '' )+
		                      +'</td></tr>'
		                   );
		                  row.find('.size').text(o.formatFileSize(file.size));
		                  if (file.error) {
		                      row.find('.name').text(file.name);
		                      row.find('.error').text(
		                          locale.fileupload.errors[file.error] || file.error
		                      );
		                  } else {
		                      row.find('.name a').text(file.name);
		                      if (file.thumbnail_url) {
		                          row.find('.preview').append('<a><img></a>')
		                              .find('img').prop('src', file.thumbnail_url);
		                          row.find('a').prop('rel', 'gallery');
		                      }
		                      row.find('a').prop('href', file.url);
		                      row.find('.delete button')
		                          .attr('data-type', file.delete_type)
		                          .attr('data-url', file.delete_url);
		                      row.find("td").first().append('<input type="hidden" name="'+ o.options.paramName +'" value="'+ file.url +'" />');
		                  }
		                  rows = rows.add(row);
		              });
		              var element = jQuery(o.options.uploadAreaHelper.element);
		              var handler = function(e){
		            	  e.data.rows.remove();
		            	  jQuery(this).unbind("clear-downloads",e.data.handler)
		              }
		              element.bind("clear-downloads",{handler : handler,rows : rows},handler);
		              return rows;
		          }
		          ,progressInterval : 20
		};
})(jQuery);

var UploadArea = {};

UploadArea.initializeLocale = function(localeObject){
	if(typeof(window.locale) == 'undefined'){window.locale = {}; }
	if(window.locale.fileupload == undefined){
		window.locale.fileupload = {};
	} 
	window.locale.fileupload = jQuery.extend({}, window.locale.fileupload,localeObject);
}

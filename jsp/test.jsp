<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Insert title here</title>

	<script type="text/javascript" src="/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/mootools/1.11/mootools-all-compressed.js"></script>
	<script type="text/javascript" src="/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/moodalbox/1.2.1/js/moodalbox.js"></script>
	
	<link rel="stylesheet" href="/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/moodalbox/1.2.1/css/moodalbox.css" type="text/css" media="screen" />
	
	<script type="text/javascript">
		window.addEvent('domready', function() {
			MOOdalBox.init({resizeDuration: 50, evalScripts: true, defContentsWidth: 900, defContentsHeight: 800});
		});
	</script>
</head>
<body>
	<a onclick="MOOdalBox.open('/servlet/ObjectInstanciator?idegaweb_instance_class=com.idega.block.article.component.ArticleEditor&contentItem=files/cms/article/root_page_article20080114-1509-1.article/', 'Edit article', '900 800');" href="javascript:void(0)">Edit article Link</a>
</body>
</html>
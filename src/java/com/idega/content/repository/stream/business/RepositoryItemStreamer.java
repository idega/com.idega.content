package com.idega.content.repository.stream.business;

import java.io.IOException;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import org.directwebremoting.annotations.Param;
import org.directwebremoting.annotations.RemoteMethod;
import org.directwebremoting.annotations.RemoteProxy;
import org.directwebremoting.spring.SpringCreator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.builder.business.BuilderLogicWrapper;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.repository.stream.bean.StreamData;
import com.idega.content.repository.stream.bean.StreamResult;
import com.idega.content.upload.servlet.ContentFileUploadServlet;
import com.idega.core.business.DefaultSpringBean;
import com.idega.core.component.bean.RenderedComponent;
import com.idega.dwr.business.DWRAnnotationPersistance;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.Layer;
import com.idega.presentation.Table2;
import com.idega.presentation.TableBodyRowGroup;
import com.idega.presentation.TableHeaderRowGroup;
import com.idega.presentation.TableRow;
import com.idega.presentation.text.Heading3;
import com.idega.presentation.text.Text;
import com.idega.repository.bean.RepositoryItem;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.FileUtil;
import com.idega.util.IOUtil;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;
import com.idega.util.URIUtil;

@Scope(BeanDefinition.SCOPE_SINGLETON)
@Service(RepositoryItemStreamer.BEAN_NAME)
@RemoteProxy(creator=SpringCreator.class, creatorParams={
	@Param(name="beanName", value=RepositoryItemStreamer.BEAN_NAME),
	@Param(name="javascript", value=RepositoryItemStreamer.DWR_OBJECT)
}, name=RepositoryItemStreamer.DWR_OBJECT)
public class RepositoryItemStreamer extends DefaultSpringBean implements DWRAnnotationPersistance {

	public static final String	BEAN_NAME = "repositoryItemStreamer",
								DWR_OBJECT = "RepositoryItemStreamer";

	@Autowired
	private BuilderLogicWrapper builderLogic;

	private Map<String, List<StreamResult>> getStreamHistory() {
		Map<String, List<StreamResult>> history = getCache(BEAN_NAME + "Cache");
		return history;
	}

	@RemoteMethod
	public void cleanStreamHistory(String uuid) {
		if (StringUtil.isEmpty(uuid))
			return;

		getStreamHistory().remove(uuid);
	}

	@RemoteMethod
	public RenderedComponent getStreamResults(String uuid) {
		if (StringUtil.isEmpty(uuid))
			return null;

		IWResourceBundle iwrb = ContentUtil.getBundle().getResourceBundle(CoreUtil.getIWContext());

		Layer results = new Layer();
		results.setStyleClass(BEAN_NAME + "Results");

		List<StreamResult> streamResults = getStreamHistory().get(uuid);
		if (ListUtil.isEmpty(streamResults)) {
			results.add(new Heading3(iwrb.getLocalizedString("no_information_about_streaming", "There is no information about the streaming results yet")));
		} else {
			streamResults = new ArrayList<StreamResult>(streamResults);
			String success = iwrb.getLocalizedString("succeeded", "Succeeded");
			String failure = iwrb.getLocalizedString("failed", "Failed");
			String seconds = iwrb.getLocalizedString("seconds", "seconds");

			Table2 table = new Table2();
			results.add(table);

			TableHeaderRowGroup header = table.createHeaderRowGroup();
			TableRow row = header.createRow();
			row.createCell().add(new Text(iwrb.getLocalizedString("file_name", "File name")));
			row.createCell().add(new Text(iwrb.getLocalizedString("size", "Size")));
			row.createCell().add(new Text(iwrb.getLocalizedString("took_time", "Streaming time")));
			row.createCell().add(new Text(iwrb.getLocalizedString("status", "Status")));

			TableBodyRowGroup body = table.createBodyRowGroup();
			for (StreamResult result: streamResults) {
				row = body.createRow();

				row.createCell().add(new Text(result.getName()));
				row.createCell().add(new Text(FileUtil.getHumanReadableSize(result.getSize())));
				row.createCell().add(new Text(String.valueOf(result.getTime() / 1000000000).concat(CoreConstants.SPACE).concat(seconds)));
				row.createCell().add(new Text(result.isSuccess() ? success : failure));
			}
		}

		return builderLogic.getBuilderService(getApplication().getIWApplicationContext()).getRenderedComponent(results, null);
	}

	@RemoteMethod
	public AdvancedProperty streamToServer(String server, String repositoryItem, String toFolder, boolean reCreateStructure, String uuid) {
		AdvancedProperty result = new AdvancedProperty(Boolean.FALSE.toString());
		IWResourceBundle iwrb = ContentUtil.getBundle().getResourceBundle(CoreUtil.getIWContext());

		if ((StringUtil.isEmpty(server) || "http://".equals(server) || "https://".equals(server)) || StringUtil.isEmpty(repositoryItem) || StringUtil.isEmpty(toFolder)) {
			String message = "Server, repository item or destination folder are not defined";
			getLogger().warning(message);
			result.setValue(iwrb.getLocalizedString("error_streaming_incorrect_data", message));
			return result;
		}

		String originalServer = server;

		if (!server.endsWith(CoreConstants.SLASH))
			server = server.concat(CoreConstants.SLASH);
		if (toFolder.indexOf(CoreConstants.DOT) != -1)
			//	File
			toFolder = toFolder.substring(0, toFolder.lastIndexOf(CoreConstants.SLASH));
		if (!toFolder.endsWith(CoreConstants.SLASH))
			toFolder = toFolder.concat(CoreConstants.SLASH);

		server = server.concat("servlet/ContentFileUploadServlet");
		URIUtil uri = new URIUtil(server);
		uri.setParameter(ContentConstants.UPLOADER_PATH, toFolder);
		uri.setParameter(ContentFileUploadServlet.PARAMETER_BINARY_STREAM, Boolean.TRUE.toString());
		String url = uri.getUri();
		try {
			RepositoryItem resource = getRepositoryService().getRepositoryItemAsRootUser(repositoryItem);

			if (writeItems(resource, url, toFolder, reCreateStructure, uuid)) {
				result.setId(Boolean.TRUE.toString());
				result.setValue(iwrb.getLocalizedString("success_streaming", "Data were succussefully streamed to").concat(CoreConstants.SPACE).concat(originalServer));
			} else
				result.setValue(iwrb.getLocalizedString("error_streaming", "Sorry, some error occurred while streaming data to").concat(CoreConstants.SPACE)
						.concat(originalServer));
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error streaming " + repositoryItem + " to server " + uri.getUri(), e);
		}

		return result;
	}

	private boolean writeItems(RepositoryItem resource, String uri, String toFolder, boolean reCreateStructure, String uuid) throws IOException {
		if (resource == null || !resource.exists())
			return false;

		if (resource.isCollection()) {
			Collection<RepositoryItem> childResources = resource.getChildResources();
			if (ListUtil.isEmpty(childResources))
				return true;

			for (RepositoryItem childResource: childResources) {
				if (!writeItems(childResource, uri, toFolder, reCreateStructure, uuid))
					return false;
			}
		} else {
			return writeItem(resource, uri, toFolder, reCreateStructure, uuid);
		}

		return true;
	}

	private boolean writeItem(RepositoryItem file, String uri, String toFolder, boolean reCreateStructure, String uuid) throws IOException {
		long start = System.nanoTime();

		URL url = new URL(uri);
		URLConnection conn = url.openConnection();
		if (conn instanceof HttpURLConnection) {
			HttpURLConnection connection = (HttpURLConnection) conn;

			String destination = toFolder;
			if (reCreateStructure) {
				destination = file.getPath();
				if (destination.startsWith(CoreConstants.WEBDAV_SERVLET_URI))
					destination = destination.replaceFirst(CoreConstants.WEBDAV_SERVLET_URI, CoreConstants.EMPTY);
				if (destination.indexOf(CoreConstants.DOT) != -1)
					destination = destination.substring(0, destination.lastIndexOf(CoreConstants.SLASH));
				if (!destination.endsWith(CoreConstants.SLASH))
					destination = destination.concat(CoreConstants.SLASH);
			}
			StreamData data = new StreamData(file.getName(), destination, uuid, file.getInputStream());
			byte[] objectData = IOUtil.getBytesFromObject(data);

			connection.setRequestMethod("POST");
			connection.setFixedLengthStreamingMode(objectData.length);
			connection.setRequestProperty("Content-Type", "text/plain;charset=".concat(CoreConstants.ENCODING_UTF8));
			connection.setRequestProperty("Accept", "text/plain");
			connection.setRequestProperty("Content-Length", String.valueOf(objectData.length));
			connection.setDoOutput(true);
			connection.connect();

			OutputStream os = connection.getOutputStream();
			os.write(objectData);
			os.flush();
			os.close();

			StreamResult result = IOUtil.getObjectFromInputStream(connection.getInputStream());
			if (result == null)
				return false;

			long end = System.nanoTime();
			result.setTime(end - start);

			Map<String, List<StreamResult>> history = getStreamHistory();
			List<StreamResult> results = history.get(uuid);
			if (results == null) {
				results = new ArrayList<StreamResult>();
				history.put(uuid, results);
			}
			results.add(result);

			return result.isSuccess();
		} else {
			getLogger().warning("Can not stream using connection " + conn + ". It is required to open " + HttpURLConnection.class.getName());
		}

		return false;
	}
}
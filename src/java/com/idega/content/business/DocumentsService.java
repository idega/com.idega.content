package com.idega.content.business;

import java.util.logging.Level;

import org.directwebremoting.annotations.Param;
import org.directwebremoting.annotations.RemoteMethod;
import org.directwebremoting.annotations.RemoteProxy;
import org.directwebremoting.spring.SpringCreator;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.core.business.DefaultSpringBean;
import com.idega.dwr.business.DWRAnnotationPersistance;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWContext;
import com.idega.util.CoreUtil;
import com.idega.util.StringUtil;

@Scope(BeanDefinition.SCOPE_SINGLETON)
@Service(DocumentsService.BEAN_NAME)
@RemoteProxy(creator=SpringCreator.class, creatorParams={
	@Param(name="beanName", value=DocumentsService.BEAN_NAME),
	@Param(name="javascript", value=DocumentsService.DWR_OBJECT)
}, name=DocumentsService.DWR_OBJECT)
public class DocumentsService extends DefaultSpringBean implements DWRAnnotationPersistance {

	public static final String	DWR_OBJECT = "DocumentsService",
								BEAN_NAME = "documentsService";

	@RemoteMethod
	public AdvancedProperty doDelete(String pathInRepository) {
		IWContext iwc = CoreUtil.getIWContext();
		IWResourceBundle iwrb = getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER).getResourceBundle(iwc);

		AdvancedProperty result = new AdvancedProperty(Boolean.FALSE.toString());
		if (StringUtil.isEmpty(pathInRepository)) {
			result.setValue(iwrb.getLocalizedString("path_is_not_provided", "Path is not provided"));
			return result;
		}
		if (!iwc.isLoggedOn() || !iwc.isSuperAdmin()) {
			result.setValue(iwrb.getLocalizedString("you_do_not_have_rights", "You do not have rights"));
			return result;
		}

		try {
			if (getRepositoryService().deleteAsRootUser(pathInRepository)) {
				result.setId(Boolean.TRUE.toString());
				result.setValue(iwrb.getLocalizedString("resource_was_successfully_deleted", "Resource was deleted"));
			} else
				result.setValue(iwrb.getLocalizedString("failed_to_delete_resource", "Failed to delete resource"));
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Failed to delete: " + pathInRepository, e);
			result.setValue(iwrb.getLocalizedString("failed_to_delete_resource", "Failed to delete resource"));
		}

		return result;
	}

}
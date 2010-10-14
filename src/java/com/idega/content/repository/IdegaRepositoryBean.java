/**
 * $Id: IdegaRepositoryBean.java,v 1.1 2009/01/06 15:17:25 tryggvil Exp $
 * Created in 2009 by tryggvil
 *
 * Copyright (C) 2000-2009 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.repository;

import javax.jcr.Credentials;
import javax.jcr.LoginException;
import javax.jcr.NoSuchWorkspaceException;
import javax.jcr.Repository;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;

import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.core.content.IdegaRepository;
import com.idega.util.expression.ELUtil;

/**
 * <p>
 * Central managing class of the Default JCR repository in idegaWeb.
 * </p>
 * 
 * Last modified: $Date: 2009/01/06 15:17:25 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:tryggvil@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.1 $
 */
@Scope("singleton")
@Service(IdegaRepository.SPRING_BEAN_IDENTIFIER)
public class IdegaRepositoryBean implements IdegaRepository{
	private Repository repository;
	private final static String SLIDE_REPOSITORY="slideRepository";
	
	public IdegaRepositoryBean(){
		//this.repository=(Repository) Class.forName("com.idega.slide.jcr.SlideRepository").newInstance();
		//this.repository=(Repository) ELUtil.getInstance().getBean(SlideRepository.SPRING_BEAN_IDENTIFIER);
	}
	
	protected Repository getRepository(){
		//TODO: Implement dynamic support for more repositories
		if(this.repository==null){
			this.repository=(Repository) ELUtil.getInstance().getBean(SLIDE_REPOSITORY);
		}
		return this.repository;
	}
	
	public String getDescriptor(String key) {
		return getRepository().getDescriptor(key);
	}

	public String[] getDescriptorKeys() {
		return getRepository().getDescriptorKeys();
	}

	public Session login() throws LoginException, RepositoryException {
		return getRepository().login();
	}

	public Session login(Credentials credentials, String workspaceName)
			throws LoginException, NoSuchWorkspaceException,
			RepositoryException {
		return getRepository().login(credentials, workspaceName);
	}

	public Session login(Credentials credentials) throws LoginException,
			RepositoryException {
		return getRepository().login(credentials);
	}

	public Session login(String workspaceName) throws LoginException,
			NoSuchWorkspaceException, RepositoryException {
		return getRepository().login(workspaceName);
	}

	@Override
	public boolean isStandardDescriptor(String key) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isSingleValueDescriptor(String key) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Value getDescriptorValue(String key) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Value[] getDescriptorValues(String key) {
		// TODO Auto-generated method stub
		return null;
	}
}

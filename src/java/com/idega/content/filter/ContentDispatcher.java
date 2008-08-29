package com.idega.content.filter;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import com.idega.servlet.filter.BaseFilter;

public class ContentDispatcher extends BaseFilter implements Filter {

	static final String CONTENT_REPOSITORY_SLIDE="slide";
	static final String CONTENT_REPOSITORY_JACKRABBIT="jackrabbit";

	static final String CONTENT_REPOSITORY="com.idega.content.repository";
	
	String contentRepository=CONTENT_REPOSITORY_JACKRABBIT;
	
	public void destroy() {
		// TODO Auto-generated method stub

	}

	public void doFilter(ServletRequest srequest, ServletResponse sresponse,
			FilterChain chain) throws IOException, ServletException {
		
		HttpServletRequest request = (HttpServletRequest)srequest;
		String jackrabbitBasePath = "/repository/default";
		
		if(contentRepository.equals(CONTENT_REPOSITORY_JACKRABBIT)){
			String url = getURIMinusContextPath(request);
			String newUrl = jackrabbitBasePath+url;
			RequestDispatcher dispatcher = request.getRequestDispatcher(newUrl);
			dispatcher.include(srequest, sresponse);
			
		}
		else{
			chain.doFilter(srequest, sresponse);
		}
		

	}

	public void init(FilterConfig arg0) throws ServletException {
		// TODO Auto-generated method stub

	}

}

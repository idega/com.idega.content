/*
 * Copyright 2005 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.idega.content.tree;

import org.apache.myfaces.custom.tree2.TreeTag;

/**
 * @author Sean Schofield
 * @version $Revision: 1.2 $ $Date: 2006/11/28 18:37:21 $
 */

public class TreeTagNew extends TreeTag //UIComponentBodyTagBase
{
	
    public String getComponentType()
    {
//    	System.out.println("getComponentType");
//        return "org.apache.myfaces.HtmlTree2";
//    	return "com.idega.content.HtmlTree2";
    	return "tree";
    }
/*
    public String getRendererType()
    {
//        return "org.apache.myfaces.HtmlTree2";
//    	return "com.idega.content.HtmlTree2";    	
    	return "lala";
    }
*/    
    public String getRendererType()
    {
    	System.out.println("getRendererType");
//        return "org.apache.myfaces.HtmlTree2";
//    	try {
//		throw new Exception();
//	} catch (Exception e) {
//		e.printStackTrace();
//		System.out.println("xxxxxxxxxxxxxxxxxxxxx-");
//	}    	
//    	return "tree";
    	return "org.apache.myfaces.HtmlTree2";
    }
}

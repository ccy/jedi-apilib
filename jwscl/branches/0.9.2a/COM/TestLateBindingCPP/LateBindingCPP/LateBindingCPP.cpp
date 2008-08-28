// LateBindingCPP.cpp : Definiert den Einstiegspunkt für die Konsolenanwendung.
//

#include "stdafx.h"
#include "atlbase.h"
#include "atlcom.h"
#include "atlctl.h"
#include "comdef.h"
#include "INVHELP.cpp"

//http://support.microsoft.com/?scid=kb%3Ben-us%3B238393&x=16&y=24
HRESULT AutoWrap(int autoType, VARIANT *pvResult, IDispatch *pDisp, 
      LPOLESTR ptName, int cArgs...) 
{
      // Begin variable-argument list...
      va_list marker;
      va_start(marker, cArgs);

      _ASSERT(pDisp);
      

      // Variables used...
      DISPPARAMS dp = { NULL, NULL, 0, 0 };
      DISPID dispidNamed = DISPID_PROPERTYPUT;
      DISPID dispID;
      HRESULT hr;
      char buf[200];
      char szName[200];
   
      // Convert down to ANSI
      WideCharToMultiByte(CP_ACP, 0, ptName, -1, szName, 256, NULL, NULL);
   
      // Get DISPID for name passed...
      hr = pDisp->GetIDsOfNames(IID_NULL, &ptName, 1, LOCALE_USER_DEFAULT, 
                                &dispID);
      if(FAILED(hr)) {
            sprintf(buf, 
                    "IDispatch::GetIDsOfNames(\"%s\") failed w/err0x%08lx",
                    szName, hr);
           // MessageBox(NULL, buf, "AutoWrap()", 0x10010);
            _exit(0);
            return hr;
      }
   
      // Allocate memory for arguments...
      VARIANT *pArgs = new VARIANT[cArgs+1];

      // Extract arguments...
      for(int i=0; i<cArgs; i++) {
            pArgs[i] = va_arg(marker, VARIANT);
      }
   
      // Build DISPPARAMS
      dp.cArgs = cArgs;
      dp.rgvarg = pArgs;
   
      // Handle special-case for property-puts!
      if(autoType & DISPATCH_PROPERTYPUT) {
            dp.cNamedArgs = 1;
            dp.rgdispidNamedArgs = &dispidNamed;
      }
   
      // Make the call!
      hr = pDisp->Invoke(dispID, IID_NULL, LOCALE_SYSTEM_DEFAULT, autoType, 
                         &dp, pvResult, NULL, NULL);
      if(FAILED(hr)) {
            sprintf(buf,
                    "IDispatch::Invoke(\"%s\"=%08lx) failed w/err 0x%08lx", 
                    szName, dispID, hr);
         //   MessageBox(NULL, buf, "AutoWrap()", 0x10010);
            _exit(0);
            return hr;
      }
      // End variable-argument section...
      va_end(marker);
   
      delete [] pArgs;
   
      return hr;

}


int _tmain(int argc, _TCHAR* argv[])
{
	CoInitialize(NULL);
	
	IDispatchPtr spIJwCoSid = NULL;
	CreateObject(OLESTR("JWSCLCom.JwCoSid"), &spIJwCoSid);
	
	BSTR bstr = NULL; 
	
	HRESULT hr = Invoke(spIJwCoSid, DISPATCH_METHOD, NULL, NULL, NULL,
             OLESTR("InitByName"), TEXT("ss"),OLESTR(""),OLESTR("Christian"));
	hr = Invoke(spIJwCoSid, DISPATCH_PROPERTYGET, NULL, NULL, NULL,
             OLESTR("StringSid"), TEXT("&s"),(BSTR FAR*)&bstr);

			 
	VARIANT result;
    VariantInit(&result);
	hr = Invoke(spIJwCoSid, DISPATCH_PROPERTYGET, &result, NULL, NULL,
             OLESTR("StringSid"), NULL);

	bstr = result.bstrVal;
	char buf[200];
    char szName[200];
   
    // Convert down to ANSI
    WideCharToMultiByte(CP_ACP, 0, bstr, -1, szName, 256, NULL, NULL);
   

	printf("%s",szName);

	return 0;
}


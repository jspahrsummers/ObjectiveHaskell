//
//  LoginWindowController.m
//  PhotoUploader
//
//  Created by Justin Spahr-Summers on 19.07.12.
//  Copyright (c) 2012 Justin Spahr-Summers. All rights reserved.
//

#import "LoginWindowController.h"
#import "Users_stub.h"
#import <WebKit/WebKit.h>

@interface LoginWindowController () {
	RACAsyncSubject *_loginSubscribable;
}

@property (nonatomic, weak) IBOutlet WebView *webView;
@property (nonatomic, weak) IBOutlet NSProgressIndicator *progressIndicator;
@end

@implementation LoginWindowController

#pragma mark Properties

@synthesize loginSubscribable = _loginSubscribable;

#pragma mark Lifecycle

- (id)init {
	return [self initWithWindowNibName:NSStringFromClass(self.class)];
}

- (id)initWithWindow:(NSWindow *)window {
	self = [super initWithWindow:window];
	if (self == nil) return nil;

	_loginSubscribable = [RACSubject subject];
	return self;
}

- (void)windowDidLoad {
	[super windowDidLoad];
    
	NSURLRequest *request = [NSURLRequest requestWithURL:[NSURL URLWithString:@"https://instagram.com/oauth/authorize/?client_id=4aaaec75a7e44968bd7c3d3a9ef3129b&redirect_uri=objhsexample://oauth_redirect&response_type=token"]];
	[self.webView.mainFrame loadRequest:request];
}

#pragma mark WebFrameLoadDelegate

- (void)webView:(WebView *)sender didStartProvisionalLoadForFrame:(WebFrame *)frame {
	[self.progressIndicator startAnimation:self];
	self.progressIndicator.hidden = NO;
}

- (void)webView:(WebView *)sender didFinishLoadForFrame:(WebFrame *)frame {
	self.progressIndicator.hidden = YES;
	[self.progressIndicator stopAnimation:self];
}

#pragma mark WebResourceLoadDelegate

- (NSURLRequest *)webView:(WebView *)sender resource:(id)identifier willSendRequest:(NSURLRequest *)request redirectResponse:(NSURLResponse *)redirectResponse fromDataSource:(WebDataSource *)dataSource {
	if (![request.URL.scheme isEqualToString:@"objhsexample"])
		return request;
	
	// skip 'access_token='
	NSString *accessToken = [request.URL.fragment substringFromIndex:13];

	[_loginSubscribable sendNext:accessToken];
	[_loginSubscribable sendCompleted];
	return request;
}

@end

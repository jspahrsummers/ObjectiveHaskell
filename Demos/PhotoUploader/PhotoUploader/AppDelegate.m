//
//  AppDelegate.m
//  PhotoUploader
//
//  Created by Justin Spahr-Summers on 17.07.12.
//  Copyright (c) 2012 Justin Spahr-Summers. All rights reserved.
//

#import "AppDelegate.h"
#import "EXTScope.h"
#import "LoginWindowController.h"

@interface AppDelegate ()
@property (nonatomic, assign) IBOutlet NSWindow *window;
@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
	LoginWindowController *loginWindowController = [[LoginWindowController alloc] init];

	[loginWindowController.loginSubscribable
		subscribeNext:^(NSString *accessToken){
			NSLog(@"access token: %@", accessToken);
		}
		completed:^{
			[loginWindowController close];
		}];

	[loginWindowController showWindow:self];
}

@end

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
#import "UserViewModel.h"
#import "UserWindowController.h"

@interface AppDelegate ()
@property (nonatomic, assign) IBOutlet NSWindow *window;
@property (nonatomic, strong) UserWindowController *userWindowController;
@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
	LoginWindowController *loginWindowController = [[LoginWindowController alloc] init];

	[loginWindowController.loginSubscribable
		subscribeNext:^(NSString *accessToken){
			UserViewModel *user = [[UserViewModel alloc] initWithAccessToken:accessToken];

			dispatch_async(dispatch_get_main_queue(), ^{
				self.userWindowController = [[UserWindowController alloc] initWithUser:user];
				[self.userWindowController showWindow:self];
			});
		}
		completed:^{
			[loginWindowController close];
		}];

	[loginWindowController showWindow:self];
}

@end

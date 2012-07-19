//
//  AppDelegate.m
//  PhotoUploader
//
//  Created by Justin Spahr-Summers on 17.07.12.
//  Copyright (c) 2012 Justin Spahr-Summers. All rights reserved.
//

#import "AppDelegate.h"
#import "LoginWindowController.h"

@interface AppDelegate ()
@property (nonatomic, assign) IBOutlet NSWindow *window;
@property (nonatomic, strong) NSWindowController *loginWindowController;
@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
	self.loginWindowController = [[LoginWindowController alloc] init];
	[self.loginWindowController showWindow:self];
}

@end

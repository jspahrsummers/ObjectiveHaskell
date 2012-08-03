//
//  UserWindowController.m
//  PhotoUploader
//
//  Created by Justin Spahr-Summers on 2012-08-03.
//  Copyright (c) 2012 Justin Spahr-Summers. All rights reserved.
//

#import "UserWindowController.h"
#import "UserViewModel.h"

@interface UserWindowController ()
@property (nonatomic, strong) UserViewModel *user;
@end

@implementation UserWindowController

- (id)initWithUser:(UserViewModel *)user {
	self = [self initWithWindowNibName:NSStringFromClass(self.class)];
	if (self == nil) return nil;

	// Property syntax is necessary here to trigger KVO.
	self.user = user;
	return self;
}

@end

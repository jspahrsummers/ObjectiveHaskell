//
//  UserWindowController.h
//  PhotoUploader
//
//  Created by Justin Spahr-Summers on 2012-08-03.
//  Copyright (c) 2012 Justin Spahr-Summers. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class UserViewModel;

@interface UserWindowController : NSWindowController

- (id)initWithUser:(UserViewModel *)user;

@end

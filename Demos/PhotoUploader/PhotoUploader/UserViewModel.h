//
//  UserViewModel.h
//  PhotoUploader
//
//  Created by Justin Spahr-Summers on 2012-08-03.
//  Copyright (C) 2012 Justin Spahr-Summers.
//  Released under the MIT license.
//

#import <Foundation/Foundation.h>

@interface UserViewModel : NSObject

- (id)initWithAccessToken:(NSString *)accessToken;

@property (copy, readonly) NSString *fullName;
@property (copy, readonly) NSString *username;
@property (copy, readonly) NSURL *photoURL;
@end

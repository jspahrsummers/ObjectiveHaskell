//
//  User.m
//  PhotoUploader
//
//  Created by Justin Spahr-Summers on 2012-08-03.
//  Copyright (C) 2012 Justin Spahr-Summers.
//  Released under the MIT license.
//

#import "User.h"
#import "User_stub.h"

@implementation User
@haskellDynamic(User, fullName);
@haskellDynamic(User, username);

- (NSURL *)photoURL {
	NSString *str = User_photoURL(self.haskellPointer);
	if (str == nil) return nil;

	return [NSURL URLWithString:str];
}

- (id)initWithData:(NSData *)data {
	// TODO: Handle failure to decode.
	return [self initWithHaskellPointer:User_initWithData(data)];
}

@end

//
//  User.m
//  PhotoUploader
//
//  Created by Justin Spahr-Summers on 2012-08-03.
//  Released into the public domain.
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

@end

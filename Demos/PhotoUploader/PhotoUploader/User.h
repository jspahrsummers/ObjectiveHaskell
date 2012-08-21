//
//  User.h
//  PhotoUploader
//
//  Created by Justin Spahr-Summers on 2012-08-03.
//  Copyright (C) 2012 Justin Spahr-Summers.
//  Released under the MIT license.
//

@interface User : OHModel

- (id)initWithData:(NSData *)data;

@property (nonatomic, copy, readonly) NSString *fullName;
@property (nonatomic, copy, readonly) NSString *username;
@property (nonatomic, copy, readonly) NSURL *photoURL;
@end

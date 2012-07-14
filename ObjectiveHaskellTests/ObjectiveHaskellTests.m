//
//  ObjectiveHaskellTests.m
//  ObjectiveHaskellTests
//
//  Created by Justin Spahr-Summers on 2012-07-13.
//  Released into the public domain.
//

#import "ObjectiveHaskellTests.h"
#import "FibTest_stub.h"
#import "MsgSendTest_stub.h"

@implementation ObjectiveHaskellTests

- (void)testFibonacci {
    STAssertEquals(3, fibonacci_hs(4), @"");
    STAssertEquals(5, fibonacci_hs(5), @"");
}

- (void)testMsgSend {
    NSString *str = msgSendTest(@"foo");
    STAssertEqualObjects(str, @"foo", @"");
}

@end

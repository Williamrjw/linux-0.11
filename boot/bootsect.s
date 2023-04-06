! 第一个扇区的内容
! 
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! 此文件被BIOS程序装载到内存的0x7c00处,并跳转到该处执行。
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

SETUPLEN = 4				! nr of setup-sectors
BOOTSEG  = 0x07c0			! original address of boot-sector ! 启动段的初始地址
INITSEG  = 0x9000			! we move boot here - out of the way ! 移动boot到这里
SETUPSEG = 0x9020			! setup starts here
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536).
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading

! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc
ROOT_DEV = 0x306

entry start
start:
	mov	ax,#BOOTSEG
	mov	ds,ax			! 把启动段的初始地址赋值为数据段基址DataSeg 段基址左移4位使用,就是所谓的0x7c00了
	mov	ax,#INITSEG
	mov	es,ax			! 把boot要移动的位置赋值给附加段ExtraSeg
	mov	cx,#256		   ! CX--CountReg赋值为256 
	sub	si,si				! 清零SourceIndex源变址寄存器
	sub	di,di				! 清零DestinationIndex目的变址寄存器
	rep							! 重复之后操作
	movw					! 复制一个字,省略了操作对象,从ds:si复制到es:di中,即0x07c00->0x90000
	! 重复的次数也省略了,是计数寄存器CX的大小,移动了256*2=512字节,相当于把这个部分又复制了一份
	jmpi	go,INITSEG	! 跳转到0x9000:go处执行
go:	mov	ax,cs			! 把这些都设置成cs的值0x9000
	mov	ds,ax
	mov	es,ax
! put stack at 0x9ff00. ! 把堆栈顶指针放在0x9ff00
	mov	ss,ax
	mov	sp,#0xFF00		! arbitrary value >>512

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.

load_setup:
	mov	dx,#0x0000		! drive 0, head 0 ！DH=磁头,DL=驱动器,00H~7FH：软盘；80H~0FFH：硬盘 
	mov	cx,#0x0002		! sector 2, track 0 ！CH=柱面,CL=扇区
	mov	bx,#0x0200		! address = 512, in INITSEG ！ES:BX=缓冲区地址
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors ！AH=2表示读扇区  AL=扇区数
	int	0x13			! read it  ! 中断：读取硬盘,前面为其参数
	！返回值：CF=0——操作成功,AH=00H,AL=传输的扇区数,否则,AH=状态代码,参见功能号01H中的说明
	jnc	ok_load_setup		! ok - continue
	mov	dx,#0x0000		！DL=驱动器
	mov	ax,#0x0000		! reset the diskette ！AH=0磁盘系统复位,
	int	0x13
	！返回值：CF=0——操作成功,AH=00H,否则,AH=状态代码,参见功能号01H中的说明
	j	load_setup

ok_load_setup: ! 完成加载setup.s

! Get disk drive parameters, specifically nr of sectors/track
! 获取磁盘驱动器参数,特别是扇区/磁道的 nr 

	mov	dl,#0x00			! DL=驱动器
	mov	ax,#0x0800		! AH=8 读取驱动器参数 
	int	0x13
	mov	ch,#0x00		！只保留CL的数据
	！CL的位7-6=柱面数的该2位 
	！CL的位5-0=扇区数 
	seg cs						! 指定段超越,下一句语句将使用cs作为段基址
	mov	sectors,cx			! 这两句合在一起相当于执行mov cs:sectors,cx 而不是mov ds:sectors,cx
	mov	ax,#INITSEG
	mov	es,ax					! 附加段设置到INITSEG的位置,即0x9000

! Print some inane message

	mov	ah,#0x03		! read cursor pos    	! AH=3表示读光标位置功能
	xor	bh,bh				! bh=0 						! BH=0表示页号是0
	int	0x10				! 服务中断					! 中断服务调用
	! 返回值是：CH=光标开始行　CL = 光标结束行　DH = 行　DL = 列
	
	mov	cx,#24				! 要打印的消息长度		! CX=字符串长度
	mov	bx,#0x0007		! page 0, attribute 7 (normal)	! BH=0页号 BL=属性7（正常的黑底白字）
	mov	bp,#msg1				! 打印的消息存放处 ! 串地址为ES:BP
	mov	ax,#0x1301		! write string, move cursor    ! AH=0x13表示显示字符串 AL=1表示串中不含属性值
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000) ! 加载系统

	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000
	call	read_it
	call	kill_motor

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined	！和0不相等就是已经定义了root_dev了
	seg cs
	mov	bx,sectors		！把之前存储的扇区信息导入到bx
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track
head:	.word 0			! current head
track:	.word 0			! current track

read_it:
	mov ax,es
	test ax,#0x0fff
die:	jne die			! es must be at 64kB boundary  ! 如果ZF=0 就死循环（不是0就说明不满足64kB的边界）
	xor bx,bx		! bx is starting address within segment！BX清零（为后文铺垫）
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?！加载完了吗
	jb ok1_read				！等价于JC，JANE，当CF=1跳转，进位或借位时跳转
	ret							！返回
ok1_read:		！比较后没有超出最后的段位置，可以进行段复制
	seg cs
	mov ax,sectors	！把sectors存储的值
	sub ax,sread	！减去1+4，即boot+setup扇区数量
	mov cx,ax		！移动到CX用作循环次数
	shl cx,#9		！CX左移9位
	add cx,bx		！加上偏移量BX
	jnc ok2_read	！CF=0跳转
	je ok2_read		！ZF=1跳转
	xor ax,ax		！AX清零
	sub ax,bx		！AX-BX 
	shr ax,#9		！
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ax,#0x1000
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	push ax
	push bx
	push cx
	push dx		！压栈
	mov dx,track
	mov cx,sread
	inc cx
	mov ch,dl
	mov dx,head
	mov dh,dl
	mov dl,#0
	and dx,#0x0100
	mov ah,#2
	int 0x13
	jc bad_rt
	pop dx		！弹栈
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
 ！翻译：这个程序关闭了软驱电机，这样我们就可以在已知状态下进入内核，以后就不用担心了。
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0			! 将要存放来自ok_load_setup的第一段来自cx中的内容。

msg1:				! 将要打印的消息
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55           ! 启动区标志,最后两个字节为此,则认为启动区

.text
endtext:
.data
enddata:
.bss
endbss:

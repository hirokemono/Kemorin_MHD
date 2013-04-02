!interpolate_table_org_IO.f90
!      module interpolate_table_org_IO
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!      subroutine write_interpolate_domain_org(id_file, my_rank)
!      subroutine write_interpolate_table_org(id_file)
!
!      subroutine read_interpolate_domain_org(id_file, n_rank)
!      subroutine read_interpolate_table_org(id_file)
!
!      subroutine write_interpolate_domain_org_b(id_file, my_rank)
!      subroutine write_interpolate_table_org_b(id_file)
!
!      subroutine read_interpolate_domain_org_b(id_file, n_rank)
!      subroutine read_interpolate_table_org_b(id_file)
!
      module interpolate_table_org_IO
!
      use m_precision
!
      use m_interpolate_table_org_IO
!
      implicit none
!
      character(len=255) :: character_4_read
      private :: character_4_read
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_domain_org(id_file, my_rank)
!
      integer(kind = kint), intent(in) :: id_file, my_rank
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!  domain ID '
      write(id_file,'(a)') '!  number of destination domain'
      write(id_file,'(a)') '!  domain IDs to send'
      write(id_file,'(a)') '!'
!
      write(id_file,'(i10)') my_rank
      write(id_file,'(i10)') num_dest_domain_IO
!
      if (num_dest_domain_IO .gt. 0) then
        write(id_file,'(10i10)')                                        &
     &          id_dest_domain_IO(1:num_dest_domain_IO)
      else
        write(id_file,*)
      end if
!
!
      end subroutine write_interpolate_domain_org
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_table_org(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i, inod
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!  stack by interpolation type'
      write(id_file,'(a)') '!  destinate global node ID, '
      write(id_file,'(a)') '!  local element ID for interpolation'
      write(id_file,'(a)') '!  interpolation type ID '
      write(id_file,'(a)') '!  generalized position '
      write(id_file,'(a)') '!'
!
      if (num_dest_domain_IO .gt. 0) then
        do i = 1, num_dest_domain_IO
          write(id_file,'(10i10)')                                      &
     &                istack_table_wtype_org_IO(4*i-3:4*i)
        end do
!
        do inod = 1, ntot_table_org_IO
          write(id_file,'(3i10,1p3e23.12)')                             &
     &        inod_gl_dest_4_org_IO(inod), iele_org_4_org_IO(inod),     &
     &        itype_inter_org_IO(inod), coef_inter_org_IO(inod,1:3)
        end do
!
      else
        write(id_file,*)
      end if
!
      end subroutine write_interpolate_table_org
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_org(id_file, n_rank)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: n_rank
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) n_rank
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) num_dest_domain_IO
!
      if (num_dest_domain_IO .gt. 0) then
!
        call allocate_itp_num_org_IO
!
        read(id_file,*) id_dest_domain_IO(1:num_dest_domain_IO)
!
      end if
!
      end subroutine read_interpolate_domain_org
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_org(id_file)
!
      use m_constants
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i, inod
!
!
!
      if (num_dest_domain_IO .gt. 0) then
!
        call read_stack_array(character_4_read, id_file,                &
     &      ifour, istack_table_wtype_org_IO(0) )
        do i = 2, num_dest_domain_IO
          read(id_file,*) istack_table_wtype_org_IO(4*i-3:4*i)
        end do
        ntot_table_org_IO                                               &
     &        = istack_table_wtype_org_IO(4*num_dest_domain_IO)
!
        call allocate_itp_table_org_IO
!
        do inod = 1, ntot_table_org_IO
          read(id_file,*) inod_gl_dest_4_org_IO(inod),                  &
     &        iele_org_4_org_IO(inod), itype_inter_org_IO(inod),        &
     &        coef_inter_org_IO(inod,1:3)
        end do
!
      end if
!
      end subroutine read_interpolate_table_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_domain_org_b(id_file, my_rank)
!
      integer(kind = kint), intent(in) :: id_file, my_rank
!
!
      write(id_file) my_rank
      write(id_file) num_dest_domain_IO
!
      if (num_dest_domain_IO .gt. 0) then
        write(id_file) id_dest_domain_IO(1:num_dest_domain_IO)
      end if
!
!
      end subroutine write_interpolate_domain_org_b
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_table_org_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i
!
      if (num_dest_domain_IO .gt. 0) then
        write(id_file) istack_table_wtype_org_IO(0:num_dest_domain_IO)
!
        write(id_file) inod_gl_dest_4_org_IO(1:ntot_table_org_IO)
        write(id_file) iele_org_4_org_IO(1:ntot_table_org_IO)
        write(id_file) inod_gl_dest_4_org_IO(1:ntot_table_org_IO)
        do i = 1, 3
          write(id_file) coef_inter_org_IO(1:ntot_table_org_IO,i)
        end do
!
      end if
!
      end subroutine write_interpolate_table_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_org_b(id_file, n_rank)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: n_rank
!
!
      read(id_file) n_rank
      read(id_file) num_dest_domain_IO
!
      if (num_dest_domain_IO .gt. 0) then
!
        call allocate_itp_num_org_IO
!
        read(id_file) id_dest_domain_IO(1:num_dest_domain_IO)
!
      end if
!
      end subroutine read_interpolate_domain_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_org_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i
!
!
      if (num_dest_domain_IO .gt. 0) then
        read(id_file) istack_table_wtype_org_IO(0:num_dest_domain_IO)
!
        ntot_table_org_IO                                               &
     &        = istack_table_wtype_org_IO(4*num_dest_domain_IO)
!
        call allocate_itp_table_org_IO
!
        read(id_file) inod_gl_dest_4_org_IO(1:ntot_table_org_IO)
        read(id_file) iele_org_4_org_IO(1:ntot_table_org_IO)
        read(id_file) inod_gl_dest_4_org_IO(1:ntot_table_org_IO)
        do i = 1, 3
          read(id_file) coef_inter_org_IO(1:ntot_table_org_IO,i)
        end do
!
      end if
!
      end subroutine read_interpolate_table_org_b
!
!-----------------------------------------------------------------------
!
      end module interpolate_table_org_IO

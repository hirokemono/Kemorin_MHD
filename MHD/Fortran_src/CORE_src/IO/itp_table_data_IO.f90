!>@file   itp_table_data_IO.f90
!!@brief  module itp_table_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006
!
!>@brief Routines for ASCII group data IO
!!
!!@verbatim
!!      subroutine write_interpolate_table_org(id_file, my_rank)
!!      subroutine write_interpolate_coefs_org(id_file)
!!
!!      subroutine read_interpolate_domain_org(id_file, n_rank)
!!      subroutine read_interpolate_table_org(id_file)
!!      subroutine read_interpolate_coefs_org(id_file)
!!
!!      subroutine write_interpolate_table_dest(id_file, my_rank)
!!      subroutine write_interpolate_coefs_dest(id_file)
!!
!!      subroutine read_interpolate_domain_dest(id_file, n_rank)
!!      subroutine read_interpolate_table_dest(id_file)
!!      subroutine read_interpolate_coefs_dest(id_file)
!!@endverbatim
!
      module itp_table_data_IO
!
      use m_precision
      use m_constants
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
      subroutine write_interpolate_table_org(id_file, my_rank)
!
      use m_interpolate_table_org_IO
!
      integer(kind = kint), intent(in) :: id_file, my_rank
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!  domain ID '
      write(id_file,'(a)') '!  number of domain to export'
      write(id_file,'(a)') '!  domain IDs to export'
      write(id_file,'(a)') '!'
!
      write(id_file,'(i16)') my_rank
      write(id_file,'(i16)') num_dest_domain_IO
!
      if (num_dest_domain_IO .gt. 0) then
        write(id_file,'(10i16)')                                        &
     &          id_dest_domain_IO(1:num_dest_domain_IO)
      else
        write(id_file,*)
      end if
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!  stack of node to export'
      write(id_file,'(a)') '!  exported node ID'
      write(id_file,'(a)') '!'
!
      if (num_dest_domain_IO .gt. 0) then
        write(id_file,'(8i16)')                                         &
              istack_nod_table_org_IO(1:num_dest_domain_IO)
        write(id_file,'(8i16)')                                         &
     &        IO_itp_org%inod_itp_send(1:IO_itp_org%ntot_table_org)
!
      else
        write(id_file,*)
      end if
!
!
      end subroutine write_interpolate_table_org
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_coefs_org(id_file)
!
      use m_interpolate_table_org_IO
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: inod
!
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
        write(id_file,'(4i16)') istack_itp_type_org_IO(1:4)
!
        do inod = 1, IO_itp_org%ntot_table_org
          write(id_file,'(3i16,1p3E25.15e3)')                           &
     &        IO_itp_org%inod_gl_dest_4_org(inod),                      &
     &        IO_itp_org%iele_org_4_org(inod),                          &
     &        IO_itp_org%itype_inter_org(inod),                         &
     &        IO_itp_org%coef_inter_org(inod,1:3)
        end do
!
      else
        write(id_file,*)
      end if
!
      end subroutine write_interpolate_coefs_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_org(id_file, n_rank)
!
      use m_interpolate_table_org_IO
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
        call allocate_itp_num_org_IO
        read(id_file,*) id_dest_domain_IO(1:num_dest_domain_IO)
      end if
!
      end subroutine read_interpolate_domain_org
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_org(id_file)
!
      use m_interpolate_table_org_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
!
      if (num_dest_domain_IO .eq. 0) return
!
      call read_stack_array(character_4_read, id_file,                  &
     &      num_dest_domain_IO, istack_nod_table_org_IO)
      IO_itp_org%ntot_table_org                                         &
     &     = istack_nod_table_org_IO(num_dest_domain_IO)
!
      call alloc_itp_table_org(IO_itp_org)
!
      read(id_file,*)                                                   &
     &      IO_itp_org%inod_itp_send(1:IO_itp_org%ntot_table_org)
!
      end subroutine read_interpolate_table_org
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_coefs_org(id_file)
!
      use m_interpolate_table_org_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i
!
!
      if (num_dest_domain_IO .eq. 0) return
      call read_stack_array(character_4_read, id_file,                  &
     &      ifour, istack_itp_type_org_IO(0) )
!
      do i = 1, IO_itp_org%ntot_table_org
        read(id_file,*) IO_itp_org%inod_gl_dest_4_org(i),               &
     &        IO_itp_org%iele_org_4_org(i),                             &
     &        IO_itp_org%itype_inter_org(i),                            &
     &        IO_itp_org%coef_inter_org(i,1:3)
      end do
!
      end subroutine read_interpolate_coefs_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_table_dest(id_file, my_rank)
!
      use m_interpolate_table_dest_IO
!
      integer(kind = kint), intent(in) :: id_file, my_rank
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!  domain ID '
      write(id_file,'(a)') '!  number of domain to import'
      write(id_file,'(a)') '!  domain IDs to import'
      write(id_file,'(a)') '!'
!
      write(id_file,'(i16)') my_rank
      write(id_file,'(i16)') num_org_domain_IO
!
      if (num_org_domain_IO .gt. 0) then
        write(id_file,'(10i16)')                                        &
     &          id_org_domain_IO(1:num_org_domain_IO)
      else
        write(id_file,*)
      end if
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!  stack of node to import'
      write(id_file,'(a)') '!  imported node ID'
      write(id_file,'(a)') '!'
!
      if (num_org_domain_IO .gt. 0) then
        write(id_file,'(8i16)')                                         &
              istack_table_dest_IO(1:num_org_domain_IO)
        write(id_file,'(8i16)') inod_dest_IO(1:ntot_table_dest_IO)
!
      else
        write(id_file,*)
      end if
!
      end subroutine write_interpolate_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_coefs_dest(id_file)
!
      use m_interpolate_table_dest_IO
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i, inod
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!  stack by interpolation type'
      write(id_file,'(a)') '!  target global node ID, '
      write(id_file,'(a)') '!  belonged local element ID '
      write(id_file,'(a)') '!  interpolation type ID '
      write(id_file,'(a)') '!  generalized position '
      write(id_file,'(a)') '!'
!
      if (num_org_domain_IO .gt. 0) then
        do i = 1, num_org_domain_IO
          write(id_file,'(8i16)')                                       &
     &                istack_table_wtype_dest_IO(4*i-3:4*i)
        end do
!
        do inod = 1, ntot_table_dest_IO
          write(id_file,'(3i16,1p3E25.15e3)')                           &
     &        inod_global_dest_IO(inod), iele_orgin_IO(inod),           &
     &        itype_inter_dest_IO(inod), coef_inter_dest_IO(inod,1:3)
        end do
!
      else
        write(id_file,*)
      end if
!
      end subroutine write_interpolate_coefs_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_dest(id_file, n_rank)
!
      use m_interpolate_table_dest_IO
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
      read(character_4_read,*) num_org_domain_IO
!
      if (num_org_domain_IO .gt. 0) then
        call allocate_itp_num_dst_IO
        read(id_file,*) id_org_domain_IO(1:num_org_domain_IO)
      end if
!
      end subroutine read_interpolate_domain_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_dest(id_file)
!
      use m_interpolate_table_dest_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
!
      if (num_org_domain_IO .eq. 0) return
!
      call read_stack_array(character_4_read, id_file,                  &
     &    num_org_domain_IO, istack_table_dest_IO)
      ntot_table_dest_IO = istack_table_dest_IO(num_org_domain_IO)
!
      call allocate_itp_nod_dst_IO
!
      read(id_file,*) inod_dest_IO(1:ntot_table_dest_IO)
!
      end subroutine read_interpolate_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_coefs_dest(id_file)
!
      use m_interpolate_table_dest_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i, inod
!
!
      if (num_org_domain_IO .eq. 0) return
!
        call read_stack_array(character_4_read, id_file,                &
     &      ifour, istack_table_wtype_dest_IO(0) )
        do i = 2, num_org_domain_IO
          read(id_file,*) istack_table_wtype_dest_IO(4*i-3:4*i)
        end do
        ntot_table_dest_IO                                              &
     &        = istack_table_wtype_dest_IO(4*num_org_domain_IO)
!
        call allocate_itp_coefs_dst_IO
!
        do inod = 1, ntot_table_dest_IO
          read(id_file,*) inod_global_dest_IO(inod),                    &
     &        iele_orgin_IO(inod), itype_inter_dest_IO(inod),           &
     &        coef_inter_dest_IO(inod,1:3)
        end do
!
      end subroutine read_interpolate_coefs_dest
!
!-----------------------------------------------------------------------
!
      end module itp_table_data_IO

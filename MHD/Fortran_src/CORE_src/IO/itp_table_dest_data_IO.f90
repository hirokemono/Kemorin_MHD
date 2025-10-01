!>@file   itp_table_dest_data_IO.f90
!!@brief  module itp_table_dest_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006
!!
!>@brief Routines for ASCII group data IO
!!
!!@verbatim
!!      subroutine write_interpolate_table_dest                         &
!!     &         (id_file, id_rank, IO_itp_dest)
!!      subroutine write_interpolate_idx_dest                           &
!!     &         (id_file, IO_itp_dest, IO_itp_c_dest)
!!      subroutine write_interpolate_coefs_dest                         &
!!     &         (id_file, IO_itp_dest, IO_itp_c_dest)
!!        type(interpolate_table_dest), intent(in) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!!
!!      subroutine read_interpolate_domain_dest                         &
!!     &         (id_file, n_rank, IO_itp_dest, iend)
!!      subroutine read_interpolate_table_dest                          &
!!     &         (id_file, IO_itp_dest, iend)
!!      subroutine read_interpolate_idx_dest                            &
!!     &         (id_file, IO_itp_dest, IO_itp_c_dest, iend)
!!      subroutine read_interpolate_coefs_dest                          &
!!     &         (id_file, IO_itp_dest, IO_itp_c_dest, iend)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!        integer(kind = kint), intent(inout) :: iend
!!@endverbatim
!
      module itp_table_dest_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_interpolation_data_labels
!
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
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
      subroutine write_interpolate_table_dest                           &
     &         (id_file, id_rank, IO_itp_dest)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
!
      write(id_file,'(a)',ADVANCE='NO') hd_itp_import_pe()
      write(id_file,'(i16)') id_rank
      write(id_file,'(i16)') IO_itp_dest%num_org_domain
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        write(id_file,'(10i16)')                                        &
     &          IO_itp_dest%id_org_domain(1:IO_itp_dest%num_org_domain)
      else
        write(id_file,*)
      end if
!
!
      write(id_file,'(a)',ADVANCE='NO') hd_itp_import_item()
      if (IO_itp_dest%num_org_domain .gt. 0) then
        write(id_file,'(8i16)')                                         &
          IO_itp_dest%istack_nod_tbl_dest(1:IO_itp_dest%num_org_domain)
        write(id_file,'(8i16)')                                         &
     &    IO_itp_dest%inod_dest_4_dest(1:IO_itp_dest%ntot_table_dest)
!
      else
        write(id_file,*)
      end if
!
      end subroutine write_interpolate_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_idx_dest                             &
     &         (id_file, IO_itp_dest, IO_itp_c_dest)
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
      integer(kind = kint) :: i, inod
!
      write(id_file,'(a)',ADVANCE='NO') hd_itp_dest_coef()
      if (IO_itp_dest%num_org_domain .gt. 0) then
        do i = 1, IO_itp_dest%num_org_domain
          write(id_file,'(8i16)')                                       &
     &             IO_itp_c_dest%istack_nod_tbl_wtype_dest(4*i-3:4*i)
        end do
!
        do inod = 1, IO_itp_dest%ntot_table_dest
          write(id_file,'(3i16)')                                       &
     &        IO_itp_c_dest%inod_gl_dest(inod),                         &
     &        IO_itp_c_dest%iele_org_4_dest(inod),                      &
     &        IO_itp_c_dest%itype_inter_dest(inod)
        end do
!
      else
        write(id_file,*)
      end if
!
      end subroutine write_interpolate_idx_dest
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_coefs_dest                           &
     &         (id_file, IO_itp_dest, IO_itp_c_dest)
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
      integer(kind = kint) :: i, inod
!
      write(id_file,'(a)',ADVANCE='NO') hd_itp_dest_coef()
      if (IO_itp_dest%num_org_domain .gt. 0) then
        do i = 1, IO_itp_dest%num_org_domain
          write(id_file,'(8i16)')                                       &
     &             IO_itp_c_dest%istack_nod_tbl_wtype_dest(4*i-3:4*i)
        end do
!
        do inod = 1, IO_itp_dest%ntot_table_dest
          write(id_file,'(3i16,1p3E25.15e3)')                           &
     &        IO_itp_c_dest%inod_gl_dest(inod),                         &
     &        IO_itp_c_dest%iele_org_4_dest(inod),                      &
     &        IO_itp_c_dest%itype_inter_dest(inod),                     &
     &        IO_itp_c_dest%coef_inter_dest(inod,1:3)
        end do
!
      else
        write(id_file,*)
      end if
!
      end subroutine write_interpolate_coefs_dest
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_dest                           &
     &         (id_file, n_rank, IO_itp_dest, iend)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: n_rank
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      integer(kind = kint), intent(inout) :: iend
!
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) n_rank
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) IO_itp_dest%num_org_domain
!
      call alloc_itp_num_dest(IO_itp_dest)
      if (IO_itp_dest%num_org_domain .gt. 0) then
        read(id_file,*)                                                 &
     &        IO_itp_dest%id_org_domain(1:IO_itp_dest%num_org_domain)
      end if
!
      end subroutine read_interpolate_domain_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_dest                            &
     &         (id_file, IO_itp_dest, iend)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      integer(kind = kint), intent(inout) :: iend
!
!
      if(IO_itp_dest%num_org_domain .gt. 0) then
        call read_stack_array                                           &
     &     (character_4_read, id_file, IO_itp_dest%num_org_domain,      &
     &      IO_itp_dest%istack_nod_tbl_dest, iend)
        if(iend .gt. 0) return
      end if
!
      IO_itp_dest%ntot_table_dest                                       &
     &   = IO_itp_dest%istack_nod_tbl_dest(IO_itp_dest%num_org_domain)
      call alloc_itp_table_dest(IO_itp_dest)
!
      if(IO_itp_dest%ntot_table_dest .le. 0) return
      read(id_file,*)                                                   &
     &     IO_itp_dest%inod_dest_4_dest(1:IO_itp_dest%ntot_table_dest)
!
      end subroutine read_interpolate_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_idx_dest                              &
     &         (id_file, IO_itp_dest, IO_itp_c_dest, iend)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
      integer(kind = kint), intent(inout) :: iend
!
      integer(kind = kint) :: i, inod, num
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
        call alloc_itp_coef_stack                                       &
     &     (IO_itp_dest%num_org_domain, IO_itp_c_dest)
!
        call read_stack_array(character_4_read, id_file, ifour,         &
     &      IO_itp_c_dest%istack_nod_tbl_wtype_dest(0:ifour), iend)
        if(iend .gt. 0) return
        do i = 2, IO_itp_dest%num_org_domain
          read(id_file,*)                                               &
     &             IO_itp_c_dest%istack_nod_tbl_wtype_dest(4*i-3:4*i)
        end do
        num = 4*IO_itp_dest%num_org_domain
        IO_itp_dest%ntot_table_dest                                     &
     &    = IO_itp_c_dest%istack_nod_tbl_wtype_dest(num)
!
        call alloc_itp_coef_dest(IO_itp_dest, IO_itp_c_dest)
!
        do inod = 1, IO_itp_dest%ntot_table_dest
          read(id_file,*) IO_itp_c_dest%inod_gl_dest(inod),             &
     &        IO_itp_c_dest%iele_org_4_dest(inod),                      &
     &        IO_itp_c_dest%itype_inter_dest(inod)
        end do
!
      end subroutine read_interpolate_idx_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_coefs_dest                            &
     &         (id_file, IO_itp_dest, IO_itp_c_dest, iend)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
      integer(kind = kint), intent(inout) :: iend
!
      integer(kind = kint) :: i, inod, num
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
        call alloc_itp_coef_stack                                       &
     &     (IO_itp_dest%num_org_domain, IO_itp_c_dest)
!
        call read_stack_array(character_4_read, id_file, ifour,         &
     &      IO_itp_c_dest%istack_nod_tbl_wtype_dest(0:ifour), iend)
        if(iend .gt. 0) return
        do i = 2, IO_itp_dest%num_org_domain
          read(id_file,*)                                               &
     &             IO_itp_c_dest%istack_nod_tbl_wtype_dest(4*i-3:4*i)
        end do
        num = 4*IO_itp_dest%num_org_domain
        IO_itp_dest%ntot_table_dest                                     &
     &    = IO_itp_c_dest%istack_nod_tbl_wtype_dest(num)
!
        call alloc_itp_coef_dest(IO_itp_dest, IO_itp_c_dest)
!
        do inod = 1, IO_itp_dest%ntot_table_dest
          read(id_file,*) IO_itp_c_dest%inod_gl_dest(inod),             &
     &        IO_itp_c_dest%iele_org_4_dest(inod),                      &
     &        IO_itp_c_dest%itype_inter_dest(inod),                     &
     &        IO_itp_c_dest%coef_inter_dest(inod,1:3)
        end do
!
      end subroutine read_interpolate_coefs_dest
!
!-----------------------------------------------------------------------
!
      end module itp_table_dest_data_IO

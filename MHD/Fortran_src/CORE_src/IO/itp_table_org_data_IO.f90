!>@file   itp_table_org_data_IO.f90
!!@brief  module itp_table_org_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006
!!
!>@brief Routines for ASCII group data IO
!!
!!@verbatim
!!      subroutine write_interpolate_table_org                          &
!!     &         (id_file, id_rank, IO_itp_org)
!!      subroutine write_interpolate_coefs_org(id_file, IO_itp_org)
!!      subroutine write_interpolate_idx_org(id_file, IO_itp_org)
!!        type(interpolate_table_org), intent(in) :: IO_itp_org
!!
!!      subroutine read_interpolate_domain_org                          &
!!     &         (id_file, n_rank, IO_itp_org, iend)
!!      subroutine read_interpolate_table_org(id_file, IO_itp_org, iend)
!!      subroutine read_interpolate_coefs_org(id_file, IO_itp_org, iend)
!!      subroutine read_interpolate_idx_org(id_file, IO_itp_org, iend)
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!@endverbatim
!
      module itp_table_org_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_interpolation_data_labels
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
      subroutine write_interpolate_table_org                            &
     &         (id_file, id_rank, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
!
      write(id_file,'(a)',ADVANCE='NO') hd_itp_export_pe()
      write(id_file,'(i16)') id_rank
      write(id_file,'(i16)') IO_itp_org%num_dest_domain
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        write(id_file,'(10i16)')                                        &
     &          IO_itp_org%id_dest_domain(1:IO_itp_org%num_dest_domain)
      else
        write(id_file,*)
      end if
!
!
      write(id_file,'(a)',ADVANCE='NO') hd_itp_export_item()
      if (IO_itp_org%num_dest_domain .gt. 0) then
        write(id_file,'(8i16)')                                         &
            IO_itp_org%istack_nod_tbl_org(1:IO_itp_org%num_dest_domain)
        write(id_file,'(8i16)')                                         &
     &      IO_itp_org%inod_itp_send(1:IO_itp_org%ntot_table_org)
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
      subroutine write_interpolate_coefs_org(id_file, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
      integer(kind = kint) :: inod
!
!
      write(id_file,'(a)',ADVANCE='NO') hd_itp_export_coef()
      if (IO_itp_org%num_dest_domain .gt. 0) then
        write(id_file,'(4i16)') IO_itp_org%istack_itp_type_org(1:4)
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
!
      subroutine write_interpolate_idx_org(id_file, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
      integer(kind = kint) :: inod
!
!
      write(id_file,'(a)',ADVANCE='NO') hd_itp_export_coef()
      if (IO_itp_org%num_dest_domain .gt. 0) then
        write(id_file,'(4i16)') IO_itp_org%istack_itp_type_org(1:4)
!
        do inod = 1, IO_itp_org%ntot_table_org
          write(id_file,'(3i16)')                                       &
     &        IO_itp_org%inod_gl_dest_4_org(inod),                      &
     &        IO_itp_org%iele_org_4_org(inod),                          &
     &        IO_itp_org%itype_inter_org(inod)
        end do
!
      else
        write(id_file,*)
      end if
!
      end subroutine write_interpolate_idx_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_org                            &
     &         (id_file, n_rank, IO_itp_org, iend)
!
      use t_interpolate_tbl_org
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: n_rank
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      integer(kind = kint), intent(inout) :: iend
!
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) n_rank
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) return
      read(character_4_read,*) IO_itp_org%num_dest_domain
!
      call alloc_itp_num_org(np_smp, IO_itp_org)
      if (IO_itp_org%num_dest_domain .gt. 0) then
        read(id_file,*)                                                 &
     &        IO_itp_org%id_dest_domain(1:IO_itp_org%num_dest_domain)
      end if
!
      end subroutine read_interpolate_domain_org
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_org(id_file, IO_itp_org, iend)
!
      use t_interpolate_tbl_org
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      integer(kind = kint), intent(inout) :: iend
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
!
      call read_stack_array(character_4_read, id_file,                  &
     &    IO_itp_org%num_dest_domain, IO_itp_org%istack_nod_tbl_org,    &
     &    iend)
      if(iend .gt. 0) return
!
      IO_itp_org%ntot_table_org                                         &
     &     = IO_itp_org%istack_nod_tbl_org(IO_itp_org%num_dest_domain)
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
      subroutine read_interpolate_coefs_org(id_file, IO_itp_org, iend)
!
      use t_interpolate_tbl_org
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      integer(kind = kint), intent(inout) :: iend
!
      integer(kind = kint) :: i
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
      call read_stack_array(character_4_read, id_file,                  &
     &      ifour, IO_itp_org%istack_itp_type_org(0:4), iend)
      if(iend .gt. 0) return
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
!
      subroutine read_interpolate_idx_org(id_file, IO_itp_org, iend)
!
      use t_interpolate_tbl_org
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      integer(kind = kint), intent(inout) :: iend
!
      integer(kind = kint) :: i
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
      call read_stack_array(character_4_read, id_file,                  &
     &      ifour, IO_itp_org%istack_itp_type_org(0:4), iend)
      if(iend .gt. 0) return
!
      do i = 1, IO_itp_org%ntot_table_org
        read(id_file,*) IO_itp_org%inod_gl_dest_4_org(i),               &
     &        IO_itp_org%iele_org_4_org(i),                             &
     &        IO_itp_org%itype_inter_org(i)
      end do
!
      end subroutine read_interpolate_idx_org
!
!-----------------------------------------------------------------------
!
      end module itp_table_org_data_IO

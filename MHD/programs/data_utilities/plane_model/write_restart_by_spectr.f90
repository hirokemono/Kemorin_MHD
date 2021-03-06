!
!     module write_restart_by_spectr
!
!      Written by H. Matsui
!      Modified by H. Matsui on June, 2007
!
!!      subroutine allocate_rst_by_plane_sp(nnod, ndir)
!!      subroutine deallocate_rst_by_plane_sp
!!      subroutine plane_nnod_stack_4_IO(num_pe, subdomain)
!!      subroutine s_write_restart_by_spectr                            &
!!     &         (ip, num_pe, nnod, merged_fld, t_IO)
!
      module write_restart_by_spectr
!
      use m_precision
      use t_file_IO_parameter
      use t_field_data_IO
!
      implicit none
!
!
      real(kind = kreal), allocatable :: rst_from_sp(:,:)
      type(field_IO), save :: pl_fld_IO
!
      private :: pl_fld_IO
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_rst_by_plane_sp(nnod, ndir)
!
      integer(kind = kint), intent(in) :: nnod, ndir
!
!
      allocate(rst_from_sp(nnod,ndir))
      rst_from_sp = 0.0e0
!
      end subroutine allocate_rst_by_plane_sp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_rst_by_plane_sp
!
      deallocate(rst_from_sp)
!
      end subroutine deallocate_rst_by_plane_sp
!
!  ---------------------------------------------------------------------
!
      subroutine plane_nnod_stack_4_IO(num_pe, subdomain)
!
      use t_mesh_data
!
      integer, intent(in) :: num_pe
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
!
      integer :: ip
!
      call alloc_merged_field_stack(num_pe, pl_fld_IO)
!
      pl_fld_IO%istack_numnod_IO(0) = 0
      do ip = 1, num_pe
        pl_fld_IO%istack_numnod_IO(ip)                                  &
     &      = pl_fld_IO%istack_numnod_IO(ip-1)                          &
     &       + subdomain(ip)%node%numnod
      end do
!
      end subroutine plane_nnod_stack_4_IO
!
!  ---------------------------------------------------------------------
!
      subroutine s_write_restart_by_spectr                              &
     &         (ip, num_pe, nnod, merged_fld, t_IO)
!
      use m_constants
      use m_field_file_format
      use t_phys_data
      use t_time_data
      use set_list_4_FFT
      use set_field_to_restart
      use set_restart_data
      use set_field_file_names
      use field_file_IO
!
      integer(kind=kint), intent(in) :: ip, nnod
      integer, intent(in) :: num_pe
      type(phys_data), intent(in) :: merged_fld
!
      type(time_data), intent(inout) :: t_IO
!
      integer :: id_rank
      character(len=kchara) :: file_name
!
!
      id_rank = int(ip - 1)
!
      call simple_init_fld_name_to_rst(nnod, merged_fld, pl_fld_IO)
      call simple_copy_fld_dat_to_rst_IO                                &
     &   (nnod, merged_fld%ntot_phys, rst_from_sp,                      &
     &    pl_fld_IO%ntot_comp_IO, pl_fld_IO%nnod_IO, pl_fld_IO%d_IO)
!
      file_name = set_FEM_fld_file_name(rst_head_plane,                 &
     &           iflag_ascii, id_rank, izero)
      call write_step_field_file(file_name, id_rank, t_IO, pl_fld_IO)
!
      call dealloc_merged_field_stack(pl_fld_IO)
      call dealloc_phys_name_IO(pl_fld_IO)
      call dealloc_phys_data_IO(pl_fld_IO)
!
      end subroutine s_write_restart_by_spectr
!
!  ---------------------------------------------------------------------
!
      end module write_restart_by_spectr

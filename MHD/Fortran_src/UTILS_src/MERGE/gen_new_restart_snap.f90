!
!     module gen_new_restart_snap
!
!      Written by H.Matsui
!
!      subroutine count_restart_data_fields
!      subroutine generate_new_restart_snap(istep)
!      subroutine update_restart_file(istep)
!
!      subroutine delete_restart_files(istep)
!      subroutine delete_old_restart(istep)
!
      module gen_new_restart_snap
!
      use m_precision
      use m_constants
      use m_control_param_merge
      use m_geometry_data_4_merge
      use m_file_format_switch
      use m_field_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_restart_data_fields
!
!
      merged_fld%num_phys =  num_phys_data_IO
      merged_fld%ntot_phys = ntot_phys_data_IO
!
      call allocate_merged_field_name
      call allocate_merged_field_data
!
      merged_fld%phys_name =        phys_data_name_IO
      merged_fld%num_component =    num_phys_comp_IO
      merged_fld%istack_component = istack_phys_comp_IO
!
      end subroutine count_restart_data_fields
!
!  ---------------------------------------------------------------------
!
      subroutine generate_new_restart_snap(istep)
!
      use m_2nd_geometry_4_merge
!
      use set_merged_restart_data
      use field_IO_select
!
      integer (kind = kint), intent(in) :: istep
      integer (kind = kint) :: ip, my_rank
!
!
      phys_file_head = org_rst_head
      iflag_field_data_fmt = iorg_rst_file_fmt
      do ip = 1, num_pe
        my_rank = ip - 1
!
        numgrid_phys_IO = subdomain(ip)%node%numnod
        call allocate_phys_data_IO
        call sel_read_step_FEM_field_file(my_rank, istep)
        call set_restart_data_2_merge(ip)
!
        call deallocate_phys_data_IO
      end do
!
!   re-scaling for magnetic field
!
      call rescale_4_magne
!
!   output new restart data
!
      phys_file_head = new_rst_head
      do ip = 1, num_pe2
        my_rank = ip - 1
!
        numgrid_phys_IO =   subdomains_2(ip)%node%numnod
        call allocate_phys_data_IO
        call set_new_restart_data(ip)
!
        iflag_field_data_fmt = inew_rst_file_fmt
        call sel_write_step_FEM_field_file(my_rank, istep)
        call deallocate_phys_data_IO
      end do
!
      end subroutine generate_new_restart_snap
!
!  ---------------------------------------------------------------------
!
      subroutine update_restart_file(istep)
!
      use m_2nd_geometry_4_merge
!
      use set_merged_restart_data
      use field_IO_select
      use input_old_file_sel_4_zlib
!
      integer (kind = kint), intent(in) :: istep
      integer (kind = kint) :: ip, my_rank
!
!
      phys_file_head = org_rst_head
      iflag_field_data_fmt = iorg_rst_file_fmt
      do ip = 1, num_pe
        my_rank = ip - 1
!
        numgrid_phys_IO = subdomain(ip)%node%numnod
        call allocate_phys_data_IO
!
        call sel_read_rst_file(my_rank, istep)
!
        call set_restart_data_2_merge(ip)
!
        call deallocate_phys_data_IO
      end do
!
!   re-scaling for magnetic field
!
      call rescale_4_magne
!
!   output new restart data
!
      phys_file_head = new_rst_head
      do ip = 1, num_pe2
        my_rank = ip - 1
!
        numgrid_phys_IO =   subdomains_2(ip)%node%numnod
        call allocate_phys_data_IO
!
        call set_new_restart_data(ip)
!
        iflag_field_data_fmt = inew_rst_file_fmt
        call sel_write_step_FEM_field_file(my_rank, istep)
        call deallocate_phys_data_IO
      end do
!
      end subroutine update_restart_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine delete_restart_files(istep)
!
      use set_field_file_names
!
      integer (kind = kint), intent(in) :: istep
!
!
      phys_file_head = org_rst_head
      call delete_FEM_fld_file(iorg_rst_file_fmt,  num_pe, istep)
!
      end subroutine delete_restart_files
!
! -----------------------------------------------------------------------
!
      subroutine delete_old_restart(istep)
!
      use delete_data_files
      use set_parallel_file_name
!
      integer (kind = kint), intent(in) :: istep
      character(len=kchara) :: fname_c
!
!
      phys_file_head = org_rst_head
      call add_int_suffix(istep, phys_file_head, fname_c)
      call delete_parallel_files(iorg_rst_file_fmt,  num_pe, fname_c)
!
      end subroutine delete_old_restart
!
! -----------------------------------------------------------------------
!
      end module gen_new_restart_snap

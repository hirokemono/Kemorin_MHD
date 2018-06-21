!output_newdomain_ucd.f90
!      module output_newdomain_ucd
!
!      Written by H. Matsui on Feb., 2007
!
!!      subroutine assemble_2nd_udt_mesh                                &
!!     &         (ucd_param, merged, num_pe2, subdomains_2, ucd)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(mesh_geometry), intent(in) :: merged
!!        type(mesh_geometry), intent(in) :: subdomains_2(num_pe2)
!!        type(phys_data), intent(in) :: merged_fld
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
!
      module output_newdomain_ucd
!
      use m_precision
!
      use m_constants
      use m_control_param_merge
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_ucd_data
      use t_file_IO_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine assemble_2nd_udt_mesh                                  &
     &         (ucd_param, merged, my_rank, new_mesh, ucd)
!
      use m_file_format_switch
      use set_ucd_data_to_type
      use ucd_IO_select
!
      integer(kind = kint), intent(in)  :: my_rank
      type(field_IO_params), intent(in) :: ucd_param
      type(mesh_geometry), intent(in) :: merged
      type(mesh_geometry), intent(in) :: new_mesh
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_global_mesh_2_ucd(new_mesh%node, new_mesh%ele, ucd)
!
      call sel_write_grd_file(my_rank, ucd_param, ucd)
!
      call deallocate_ucd_node(ucd)
      call deallocate_ucd_ele(ucd)
!
      end subroutine assemble_2nd_udt_mesh
!
!  ---------------------------------------------------------------------
!
      end module output_newdomain_ucd

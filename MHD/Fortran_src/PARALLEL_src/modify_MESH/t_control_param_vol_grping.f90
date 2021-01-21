!>@file   t_control_param_vol_grping.f90
!!@brief  module t_control_param_vol_grping
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!
!!     subroutine set_ctl_param_vol_grping                              &
!!    &         (default_newmesh_head, new_part_ctl, part_param)
!!        character(len = kchara),intent(in) :: default_newmesh_head
!!        type(new_patition_control), intent(in) :: new_part_ctl
!!        type(volume_partioning_param), intent(inout) :: part_param
!!@endverbatim
!
      module t_control_param_vol_grping
!
      use m_precision
      use calypso_mpi
!
      use t_file_IO_parameter
      use t_ctl_data_volume_grouping
!
      implicit none
!
!>        Structure for repartitioning parameters
      type volume_partioning_param
!>        Data transfer table file parameters
        type(field_IO_params) :: trans_tbl_file
!
!>        number of subdomains for original partition
        integer(kind = kint) :: org_nprocs
!>        number of subdomains for new partition
        integer(kind = kint) :: new_nprocs
!>        number of subdomains in each direction for new partition
        integer(kind = kint) :: ndomain_eb(3)
!>        number of blocks in each direction for new partition
        integer(kind = kint) :: ndivide_eb(3)
!
!>        Sleeve size level
        integer(kind = kint) :: num_FEM_sleeve = 1
      end type volume_partioning_param
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_param_vol_grping                               &
     &         (default_newmesh_head, new_part_ctl, part_param)
!
      use m_file_format_switch
      use set_control_platform_item
      use set_control_platform_data
      use set_num_domain_each_dir
!
      character(len = kchara),intent(in) :: default_newmesh_head
      type(new_patition_control), intent(in) :: new_part_ctl
      type(volume_partioning_param), intent(inout) :: part_param
!
!
      if(new_part_ctl%repart_table_head_ctl%iflag .le. 0) then
        part_param%trans_tbl_file%iflag_format = id_no_file
      else
        call set_parallel_file_ctl_params(default_newmesh_head,         &
     &      new_part_ctl%repart_table_head_ctl,                         &
     &      new_part_ctl%repart_table_fmt_ctl,                          &
     &      part_param%trans_tbl_file)
      end if
!
      part_param%num_FEM_sleeve = 1
      if(new_part_ctl%sleeve_level_ctl%iflag .gt. 0) then
        part_param%num_FEM_sleeve                                       &
     &      = max(new_part_ctl%sleeve_level_ctl%intvalue, 1)
      end if
!
      part_param%new_nprocs = nprocs
      call set_control_EQ_XYZ(new_part_ctl%ndomain_section_ctl,         &
     &    part_param%new_nprocs, part_param%ndomain_eb)
!
      if(new_part_ctl%ratio_of_grouping_ctl%iflag .eq. 0) then
        part_param%ndivide_eb(1:3) = 100 * part_param%ndomain_eb(1:3)
      else
        part_param%ndivide_eb(1:3) = part_param%ndomain_eb(1:3)         &
     &       * new_part_ctl%ratio_of_grouping_ctl%intvalue
      end if
!
      if(my_rank .eq. 0) then
        write(*,*) 'ndomain_eb', part_param%ndomain_eb(1:3)
        write(*,*) 'ndivide_eb', part_param%ndivide_eb(1:3)
      end if
!
      end subroutine set_ctl_param_vol_grping
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_vol_grping

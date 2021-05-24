!>@file   t_control_param_vol_grping.f90
!!@brief  module t_control_param_vol_grping
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine link_repart_masking_data(flag_mask, flag_sleeve_wk,  &
!!     &          node, nmax_mask_org, d_mask_org, vect_ref_ext,        &
!!     &          repart_WK)
!!      subroutine unlink_repart_masking_data(repart_WK)
!!        logical, intent(in) :: flag_mask, flag_sleeve_wk
!!        integer(kind = kint), intent(in) :: nmax_mask_org
!!        type(node_data), intent(in) :: node
!!        real(kind = kreal), intent(in), target                        &
!!       &                    :: vect_ref_ext(node%numnod,3)
!!        real(kind = kreal), intent(in), target                        &
!!       &                    :: d_mask_org(node%numnod,nmax_mask_org)
!!        type(volume_partioning_work), intent(inout) :: repart_WK
!!
!!      subroutine link_repart_masking_param(num_mask_org, masking_org, &
!!     &                                     part_param)
!!      subroutine unlink_repart_masking_param(part_param)
!!        integer(kind = kint), intent(in) :: num_mask_org
!!        type(masking_parameter), intent(in), target                   &
!!       &                        :: masking_org(num_mask_org)
!!        type(volume_partioning_param), intent(inout) :: part_param
!!
!!      subroutine set_ctl_param_vol_repart(viz_repart_c, part_param)
!!        integer(kind = kint), intent(in) :: num_mask_org, nmax_mask_org
!!        type(masking_parameter), intent(in), target                   &
!!       &                    :: masking_org(num_mask_org)
!!        type(node_data), intent(in) :: node
!!        real(kind = kreal), intent(in), target                        &
!!      &                        :: vect_ref_ext(node%numnod,3)
!!        real(kind = kreal), intent(in), target                        &
!!       &                    :: d_mask_org(node%numnod,nmax_mask_org)
!!        type(volume_partioning_param), intent(inout) :: part_param
!!
!!      subroutine set_ctl_param_vol_repart(viz_repart_c, part_param)
!!        type(viz_repartition_ctl), intent(in) :: viz_repart_c
!!        type(FEM_sleeve_control), intent(in) :: sleeve_c
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
      use t_ctl_param_sleeve_extend
      use t_ctl_param_masking
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &             :: default_newmesh_head = 'repartition_mesh'
!
      character(len=kchara), parameter, private                         &
     &             :: c_VOLUME_BASED = 'VOLUME_BASED'
      character(len=kchara), parameter, private                         &
     &             :: c_NODE_BASED = 'NODE_BASED'
!
      integer(kind = kint), parameter :: i_VOLUME_BASED = 0
      integer(kind = kint), parameter :: i_NODE_BASED =   1
!
!>        Structure for repartitioning parameters
      type volume_partioning_param
!>        Logical flag for repartitiong
        logical :: flag_repartition = .FALSE.
!
!>        Integer flag to output surface data
        integer(kind = kint) :: iflag_output_SURF = 0
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: viz_mesh_file
!>        Structure for new field file  paramters
        type(field_IO_params) :: viz_ucd_file

!>        Data transfer table file parameters
        type(field_IO_params) :: trans_tbl_file
!
!>        Integer flag for re-partitioning reference
        integer(kind = kint) :: iflag_repart_ref = 0
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
!>        number of maskings
        logical :: flag_mask_repart = .FALSE.
!>        number of maskings
        integer(kind = kint) :: num_mask_repart
!>        Masking data
        type(masking_parameter), pointer :: masking_repart(:)
!>        Weight to shrink excluded area
        real(kind = kreal) :: shrink = 0.1d0
!
!>      Structure of sleeve extension parameter
        type(sleeve_extension_param) :: sleeve_exp_p
      end type volume_partioning_param
!
      type volume_partioning_work
!>        number of maskings
        integer(kind = kint) :: nmax_mask_repart
!>        Pointer of field to mask
        real(kind = kreal), pointer :: d_mask(:,:)
!
!>        Work area for sleeve extension
        type(sleeve_extension_work) :: sleeve_exp_WK
      end type volume_partioning_work
!
      private :: set_ctl_param_vol_grping
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine link_repart_masking_data(flag_mask, flag_sleeve_wk,    &
     &          node, nmax_mask_org, d_mask_org, vect_ref_ext,          &
     &          repart_WK)
!
      use t_geometry_data
!
      logical, intent(in) :: flag_mask, flag_sleeve_wk
      integer(kind = kint), intent(in) :: nmax_mask_org
      type(node_data), intent(in) :: node
      real(kind = kreal), intent(in), target                            &
     &                        :: vect_ref_ext(node%numnod,3)
      real(kind = kreal), intent(in), target                            &
     &                        :: d_mask_org(node%numnod,nmax_mask_org)
!
      type(volume_partioning_work), intent(inout) :: repart_WK
!
!
      if(flag_mask) then
        repart_WK%nmax_mask_repart = nmax_mask_org
        repart_WK%d_mask =>          d_mask_org
      end if
!
      if(flag_sleeve_wk) then
        call link_sleeve_extend_ref_vect(node, vect_ref_ext,            &
     &      flag_sleeve_wk, repart_WK%sleeve_exp_WK)
      end if
!
      end subroutine link_repart_masking_data
!
!   --------------------------------------------------------------------
!
      subroutine unlink_repart_masking_data(repart_WK)
!
      type(volume_partioning_work), intent(inout) :: repart_WK
!
!
      call unlink_sleeve_extend_ref_vect(repart_WK%sleeve_exp_WK)
!
      if(associated(repart_WK%d_mask)) then
        repart_WK%nmax_mask_repart = 0
        nullify(repart_WK%d_mask)
      end if
!
      end subroutine unlink_repart_masking_data
!
!   --------------------------------------------------------------------
!
      subroutine link_repart_masking_param(num_mask_org, masking_org,   &
     &                                     part_param)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: num_mask_org
      type(masking_parameter), intent(in), target                       &
     &                        :: masking_org(num_mask_org)
!
      type(volume_partioning_param), intent(inout) :: part_param
!
      part_param%num_mask_repart = 0
      if(part_param%flag_mask_repart) then
        part_param%num_mask_repart = num_mask_org
      end if
!
      if(part_param%num_mask_repart .le. 0) return
      part_param%masking_repart => masking_org
!
      end subroutine link_repart_masking_param
!
!   --------------------------------------------------------------------
!
      subroutine unlink_repart_masking_param(part_param)
!
      type(volume_partioning_param), intent(inout) :: part_param
!
      if(associated(part_param%masking_repart) .EQV. .FALSE.) return
!
      nullify(part_param%masking_repart)
      part_param%num_mask_repart = 0
!
      end subroutine unlink_repart_masking_param
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_ctl_param_vol_repart(viz_repart_c, part_param)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
!
      use m_machine_parameter
      use m_file_format_switch
      use t_ctl_data_volume_repart
      use t_ctl_data_FEM_sleeve_size
      use set_control_platform_item
      use set_control_platform_data
      use set_ctl_parallel_platform
      use parallel_ucd_IO_select
!
      type(viz_repartition_ctl), intent(in) :: viz_repart_c
      type(volume_partioning_param), intent(inout) :: part_param
!
      integer(kind = kint) :: ierr
!
!
      call check_control_num_domains(viz_repart_c%viz_plt)
!
      if(viz_repart_c%viz_plt%mesh_file_prefix%iflag .le. 0) then
        part_param%viz_mesh_file%iflag_format = id_no_file
      else
        call set_parallel_file_ctl_params(default_newmesh_head,         &
     &      viz_repart_c%viz_plt%mesh_file_prefix,                      &
     &      viz_repart_c%viz_plt%mesh_file_fmt_ctl,                     &
     &      part_param%viz_mesh_file)
      end if
!
      call set_FEM_surface_output_flag                                  &
     &   (viz_repart_c%Fmesh_ctl, part_param%iflag_output_SURF)
!
      call set_merged_ucd_file_define(viz_repart_c%viz_plt,             &
     &                                part_param%viz_ucd_file)
!
      if(viz_repart_c%i_viz_repartition_ctl .gt. 0) then
        part_param%flag_repartition = .TRUE.
!
        call set_ctl_param_vol_grping                                   &
     &     (viz_repart_c%new_part_ctl, part_param)
      end if
!
      if(part_param%new_nprocs                                          &
     &      .ne. viz_repart_c%viz_plt%ndomain_ctl%intvalue) then
        write(e_message,'(a)')                                          &
     &      'Number of subdomains should be num. of original mesh'
        call calypso_MPI_abort(ierr_P_MPI, e_message)
      end if
!
      call set_ctl_param_sleeve_extension                               &
     &   (viz_repart_c%Fsleeve_ctl, part_param%sleeve_exp_p, ierr)
!
      end subroutine set_ctl_param_vol_repart
!
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_param_vol_grping(new_part_ctl, part_param)
!
      use m_file_format_switch
      use set_control_platform_item
      use set_control_platform_data
      use set_num_domain_each_dir
!
      type(new_patition_control), intent(in) :: new_part_ctl
      type(volume_partioning_param), intent(inout) :: part_param
!
      character(len = kchara) :: tmpchara
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
      part_param%iflag_repart_ref = i_VOLUME_BASED
      if(new_part_ctl%partition_reference_ctl%iflag .gt. 0) then
        tmpchara = new_part_ctl%partition_reference_ctl%charavalue
        if(cmp_no_case(tmpchara,c_NODE_BASED)) then
          part_param%iflag_repart_ref = i_NODE_BASED
        end if
      end if
!
      part_param%num_mask_repart = 0
      part_param%flag_mask_repart = .FALSE.
      if(new_part_ctl%masking_switch_ctl%iflag .gt. 0) then
        tmpchara = new_part_ctl%masking_switch_ctl%charavalue
        if(yes_flag(tmpchara)) then
          part_param%flag_mask_repart = .TRUE.
        end if
      end if
!
      part_param%shrink = 0.1d0
      if(new_part_ctl%masking_weight_ctl%iflag .gt. 0) then
        part_param%shrink = new_part_ctl%masking_weight_ctl%realvalue
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

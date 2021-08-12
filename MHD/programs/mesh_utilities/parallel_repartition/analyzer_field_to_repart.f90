!>@file   analyzer_field_to_repart.f90
!!@brief  module analyzer_field_to_repart
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine initialize_field_to_repart
!!      subroutine analyze_field_to_repart
!!@endverbatim
!
      module analyzer_field_to_repart
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_calypso_comm_table
      use t_control_param_repartition
      use t_time_data
      use t_VIZ_only_step_parameter
      use t_mesh_SR
!
      use m_elapsed_labels_SEND_RECV
      use m_elapsed_labels_4_REPART
      use m_work_time_4_sleeve_extend
      use m_work_time
!
      implicit none
!
      type(mesh_data), save :: fem_T
      type(communication_table) :: ele_comm_T
      type(mesh_data), save :: new_fem
      type(calypso_comm_table), save :: repart_nod_tbl1
!
!>      Structure for communicatiors for solver
      type(mesh_SR), save :: m_SR_T
!
      type(vol_partion_prog_param), save ::  part_p1
!>        Structure for new time stepping
      type(time_step_param_w_viz), save :: t_VIZ_T
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_field_to_repart
!
      use t_ctl_file_volume_grouping
!
      use m_error_IDs
      use m_file_format_switch
!
      use t_next_node_ele_4_node
      use t_jacobians
      use t_shape_functions
!
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use field_to_new_partition
      use nod_and_ele_derived_info
      use const_element_comm_tables
      use parallel_FEM_mesh_init
      use int_volume_of_single_domain
      use set_table_4_RHS_assemble
!
!
!>     Stracture for Jacobians
!
      type(new_patition_test_control) :: part_tctl1
      type(next_nod_ele_table) :: next_tbl1
      type(jacobians_type) :: jacobians1
      type(shape_finctions_at_points) :: spfs1
      type(volume_partioning_work), save :: repart_WK1
!
      type(masking_parameter), allocatable, target :: masking1(:)
      real(kind = kreal), allocatable :: d_mask_org1(:,:)
      real(kind = kreal), allocatable :: vect_ref1(:,:)
!
      integer(kind = kint) :: ierr
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_repartition
      call elpsed_label_4_sleeve_ext
      call elpsed_label_field_send_recv
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
!
!     ----- read control data
!
      call read_control_new_partition(part_tctl1)
!
      call set_control_param_repartition(part_tctl1, part_p1)
      
      call set_fixed_t_step_params_w_viz                                &
     &   (part_tctl1%t_viz_ctl, t_VIZ_T, ierr, e_message)
      call copy_delta_t(t_VIZ_T%init_d, t_VIZ_T%time_d)
!
!  --  read geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_p1%mesh_file, nprocs, fem_T)
!
!  -------------------------------
!
      call FEM_comm_initialization(fem_T%mesh, m_SR_T)
!
!  -------------------------------
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(fem_T%mesh, fem_T%group,             &
     &                             m_SR_T%SR_sig, m_SR_T%SR_i)
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
      call const_ele_comm_table                                         &
     &   (fem_T%mesh%node, fem_T%mesh%nod_comm, fem_T%mesh%ele,         &
     &    ele_comm_T, m_SR_T)
!
!  -----  Const volume of each element
      if(iflag_debug.gt.0) write(*,*) 'const_jacobian_and_single_vol'
      call const_jacobian_and_single_vol                                &
     &   (fem_T%mesh, fem_T%group, spfs1, jacobians1)
      call finalize_jac_and_single_vol                                  &
     &   (fem_T%mesh, spfs1, jacobians1)
!
!  -----  Const Neighboring information
      if(iflag_debug .gt. 0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (fem_T%mesh, next_tbl1%neib_ele, next_tbl1%neib_nod)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+5)
!
      if(iflag_debug .gt. 0) write(*,*) 'load_or_const_new_partition'
      allocate(masking1(0))
      allocate(d_mask_org1(fem_T%mesh%node%numnod,0))
      allocate(vect_ref1(fem_T%mesh%node%numnod,3))
      call link_repart_masking_param(0, masking1, part_p1%repart_p)
      call link_repart_masking_data((.FALSE.),                          &
     &    fem_T%mesh%node, izero, d_mask_org1, repart_WK1)
      call load_or_const_new_partition                                  &
     &   ((.TRUE.), part_p1%repart_p, fem_T, ele_comm_T, next_tbl1,     &
     &    vect_ref1(1,1), vect_ref1, new_fem, repart_nod_tbl1,          &
     &    repart_WK1, m_SR_T)
      call unlink_repart_masking_data(repart_WK1)
      call unlink_repart_masking_param(part_p1%repart_p)
      call dealloc_comm_table(ele_comm_T)
      deallocate(d_mask_org1, vect_ref1, masking1)
!
      call dealloc_next_nod_ele_table(next_tbl1)
      call dealloc_mesh_infomations(fem_T%mesh, fem_T%group)
!
      call set_nod_and_ele_infos(new_fem%mesh%node, new_fem%mesh%ele)
      call const_global_mesh_infos(new_fem%mesh)
!
      end subroutine initialize_field_to_repart
!
! ----------------------------------------------------------------------
!
      subroutine analyze_field_to_repart
!
      use t_IO_step_parameter
      use t_VIZ_step_parameter
      use t_ucd_data
!
      use parallel_ucd_IO_select
      use set_ucd_data
!
      use select_copy_from_recv
      use udt_to_new_partition
!
!
      type(time_data) :: t_IO
      type(ucd_data) ::  org_ucd, new_ucd
!
      integer(kind = kint) :: i_step, ist, ied
      integer(kind = kint) :: istep_ucd = 0
!
!
      call init_udt_to_new_partition(part_p1%repart_p%viz_ucd_file,     &
     &                               new_fem%mesh, new_ucd)
!
      ist = t_VIZ_T%init_d%i_time_step
      ied = t_VIZ_T%finish_d%i_end_step
      do i_step = ist, ied
        if(output_IO_flag(i_step,t_VIZ_T%ucd_step) .eqv. .FALSE.) cycle
!        if(iflag_vizs_w_fix_step(i_step, t_VIZ_T%viz_step)
!     &        .eqv. .FALSE.) cycle
!
        istep_ucd = IO_step_exc_zero_inc(i_step, t_VIZ_T%ucd_step)
        call alloc_merged_ucd_nod_stack(nprocs, org_ucd)
        call sel_read_alloc_para_udt_file                               &
     &     (istep_ucd, part_p1%org_ucd_file, t_IO, org_ucd)
!
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+4)
        call udt_field_to_new_partition(iflag_import_item,              &
     &      istep_ucd, part_p1%repart_p%viz_ucd_file, t_IO,             &
     &      new_fem%mesh, repart_nod_tbl1, org_ucd, new_ucd,            &
     &      m_SR_T%v_sol, m_SR_T%SR_sig, m_SR_T%SR_r)
        if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+4)
!
        call deallocate_ucd_phys_data(org_ucd)
        call deallocate_ucd_phys_name(org_ucd)
      end do
      call finalize_udt_to_new_partition(new_ucd)
!
!      if(my_rank .eq. 0) then
!        write(*,*) 't_VIZ_T%viz_step%PSF_t', t_VIZ_T%viz_step%PSF_t
!        write(*,*) 't_VIZ_T%viz_step%ISO_t', t_VIZ_T%viz_step%ISO_t
!        write(*,*) 't_VIZ_T%viz_step%PVR_t', t_VIZ_T%viz_step%PVR_t
!        write(*,*) 't_VIZ_T%viz_step%FLINE_t',t_VIZ_T%viz_step%FLINE_t
!        write(*,*) 't_VIZ_T%viz_step%LIC_t', t_VIZ_T%viz_step%LIC_t
!      end if
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      if(iflag_debug.gt.0) write(*,*) 'exit analyze_field_to_repart'
!
      end subroutine analyze_field_to_repart
!
! ----------------------------------------------------------------------
!
      end module analyzer_field_to_repart

!>@file   analyzer_chk_repart_by_mesh.f90
!!@brief  module analyzer_repart_by_volume
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine initialize_chk_repart_by_mesh
!!      subroutine analyze_chk_repart_by_mesh
!!@endverbatim
!
      module analyzer_chk_repart_by_mesh
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_REPART
      use m_work_time_4_sleeve_extend
      use calypso_mpi
!
      use t_mesh_data
      use t_calypso_comm_table
      use t_control_param_repartition
      use t_vector_for_solver
      use t_mesh_SR
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &               :: fname_new_part_ctl = "ctl_repartition"
      character(len=kchara), parameter                                  &
     &     :: repart_test_name = 'repart_check.dat'
!
      type(vol_partion_prog_param), save ::  part_p1
      type(mesh_data), save :: fem_T
      type(communication_table) :: ele_comm_T
      type(mesh_SR), save :: m_SR_T
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_chk_repart_by_mesh
!
      use t_next_node_ele_4_node
      use t_ctl_file_volume_grouping
      use t_const_comm_table
!
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use nod_phys_send_recv
!
!     ---------------------
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_repartition
      call elpsed_label_4_sleeve_ext
      call elapsed_label_4_ele_comm_tbl
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
!
!     ----- read control data
!
      call input_control_new_partition(fname_new_part_ctl, part_p1)
!
!  --  read geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_p1%mesh_file, nprocs, fem_T)
!
!  -------------------------------
!
      call init_nod_send_recv(fem_T%mesh,                               &
     &    m_SR_T%SR_sig, m_SR_T%SR_r, m_SR_T%SR_i, m_SR_T%SR_il)
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(fem_T%mesh, fem_T%group,             &
     &                             m_SR_T%SR_sig, m_SR_T%SR_i)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+5)
!
      end subroutine initialize_chk_repart_by_mesh
!
! ----------------------------------------------------------------------
!
      subroutine analyze_chk_repart_by_mesh
!
      use t_solver_SR
!
      use t_repart_double_numberings
      use t_para_double_numbering
!
      use calypso_mpi_int
      use m_file_format_switch
      use parallel_FEM_mesh_init
      use load_repartition_table
!
      use mpi_load_mesh_data
      use const_element_comm_tables
      use compare_mesh_structures
      use repartiton_by_volume
!
      type(calypso_comm_table), save :: part_nod_tbl2
      type(calypso_comm_table), save :: part_ele_tbl2
      type(communication_table), save :: new_ele_comm2
!
      type(mesh_data), save :: new_fem2, new_fem_f
      integer(kind = kint) :: icount_error, icou_error_gl
!
!
      if(part_p1%repart_p%trans_tbl_file%iflag_format                   &
     &     .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*) 'No file to check data transfer'
        return
      end if
!
!  --  read geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_p1%repart_p%viz_mesh_file,               &
     &                    nprocs, new_fem_f)
      call calypso_mpi_barrier
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
      if(iflag_debug.gt.0) write(*,*)' FEM_mesh_initialization'
      call FEM_mesh_initialization(new_fem_f%mesh, new_fem_f%group,     &
     &                             m_SR_T%SR_sig, m_SR_T%SR_i)
!
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
!      call const_global_numele_list(fem_T%mesh%ele)
      call const_ele_comm_table(fem_T%mesh%node, fem_T%mesh%nod_comm,   &
     &                          fem_T%mesh%ele, ele_comm_T, m_SR_T)
!
!  --  Construct repartitioned mesh
      call load_repartitoned_table_mesh((.FALSE.),                      &
     &    part_p1%repart_p, fem_T, ele_comm_T, new_fem2, new_ele_comm2, &
     &    part_nod_tbl2, part_ele_tbl2, m_SR_T)
      call dealloc_comm_table(ele_comm_T)
!
!
      call compare_node_comm_types(my_rank, new_fem_f%mesh%nod_comm,    &
     &                            new_fem2%mesh%nod_comm, icount_error)
      call calypso_mpi_reduce_one_int                                   &
     &   (icount_error, icou_error_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Compare node comm table: ',        &
      &                 icou_error_gl
!      write(*,*) my_rank, 'Compare node comm table: ', icount_error
!
      call compare_node_position(my_rank, new_fem_f%mesh%node,          &
     &                           new_fem2%mesh%node, icount_error)
      call calypso_mpi_reduce_one_int                                   &
     &   (icount_error, icou_error_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Compare node: ', icou_error_gl
!      write(*,*) my_rank, 'Compare node: ', icount_error
!
      call compare_ele_connect(my_rank, new_fem_f%mesh%ele,             &
     &    new_fem2%mesh%ele, icount_error)
      call calypso_mpi_reduce_one_int                                   &
     &   (icount_error, icou_error_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Compare element: ', icou_error_gl
!      write(*,*) my_rank, 'Compare element: ', icount_error
!
      call compare_mesh_groups(my_rank, new_fem_f%group,                &
     &                         new_fem2%group)
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_chk_repart_by_mesh
!
!  ---------------------------------------------------------------------
!
      end module analyzer_chk_repart_by_mesh

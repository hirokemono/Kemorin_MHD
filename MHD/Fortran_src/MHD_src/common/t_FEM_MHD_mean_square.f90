!>@file   t_FEM_MHD_mean_square.f90
!!        module t_FEM_MHD_mean_square
!!
!! @author H. Matsui
!! @date   Programmed in 2002
!! @n      Modified  on Jan., 2013
!!
!
!> @brief addresses for volume integrated data
!!
!!@verbatim
!!      subroutine init_FEM_MHD_mean_square(nod_fld)
!!      subroutine output_time_step_control(istep, rms_step,            &
!!     &          FEM_prm, time_d, mesh, MHD_mesh, fMHD_prop,           &
!!     &          iphys, nod_fld, iphys_ele, ele_fld, jacs,             &
!!     &          ifld_msq, rhs_mat, mhd_fem_wk, fem_msq)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(mean_square_address), intent(in) :: ifld_msq
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(mean_square_values), intent(inout) :: fem_msq
!
!!@endverbatim
!
      module t_FEM_MHD_mean_square
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
      use t_mean_square_values
      use t_mean_square_filed_list
!
      use t_FEM_control_parameter
      use t_control_parameter
      use t_time_data
      use t_flex_delta_t_data
      use t_mesh_data
      use t_geometry_data
      use t_geometry_data_MHD
      use t_jacobians
      use t_finite_element_mat
      use t_work_FEM_integration
      use t_MHD_finite_element_mat
      use t_IO_step_parameter
!
      implicit  none
!
      type FEM_MHD_mean_square
!>        Structure for mean square values
        type(mean_square_values) :: msq
!>        Structure for mean square addresses not listed in phys_address
        type(mean_square_address) :: i_msq
!
!>      strucutre of mean square data addresses
        type(mean_square_list) :: msq_list
!
!>        Structure for addresses of volume average
        type(phys_address) :: i_rms
!>        Structure for addresses of mean square
        type(phys_address) :: j_ave
      end type FEM_MHD_mean_square
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_FEM_MHD_mean_square(nod_fld, iphys, fem_sq)
!
      use calypso_mpi
      use set_mean_square_array
!
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
!      integer(kind = kint) :: i
!
      call alloc_mean_square_name(fem_sq%msq_list)
      call set_mean_square_values(nod_fld, iphys,                       &
     &    fem_sq%i_rms, fem_sq%j_ave, fem_sq%i_msq, fem_sq%msq_list)
!
      fem_sq%msq%num_rms = fem_sq%msq_list%numrms
      fem_sq%msq%num_ave = fem_sq%msq_list%numave
      call alloc_mean_square_values(fem_sq%msq)
!
!      if(iflag_debug .eq. 0) return
!      do i = 1, fem_sq%msq_list%nfield
!        write(*,'(i5,a2,a,a2,4i5)') i, '. ',                           &
!     &      trim(fem_sq%msq_list%field_name(i)), ': ',                 &
!     &      fem_sq%msq_list%ifld_msq(i), fem_sq%msq_list%ncomp_msq(i), &
!     &      fem_sq%msq_list%irms_msq(i), fem_sq%msq_list%jave_msq(i)
!      end do
!
      end subroutine init_FEM_MHD_mean_square
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine output_time_step_control(istep, rms_step,              &
     &          FEM_prm, time_d, mesh, MHD_mesh, MHD_prop,              &
     &          iphys, nod_fld, iphys_ele, ele_fld, jacs,               &
     &          rhs_mat, mhd_fem_wk, fem_sq)
!
      use calypso_mpi
      use t_mean_square_values
!
      use int_bulk
      use time_step_file_IO
!
      integer(kind = kint), intent(in) :: istep
      type(IO_step_param), intent(in) :: rms_step
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(time_data), intent(in) :: time_d
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      integer (kind = kint) :: nd
!
!
      if(output_IO_flag(istep, rms_step) .ne. 0) return
      if(my_rank .eq. 0) write(*,'(a10,i16,a10,e15.8)')                 &
     &            'i_step=', time_d%i_time_step,'time=', time_d%time
!
      call s_int_mean_squares(FEM_prm%npoint_t_evo_int,                 &
     &    mesh, MHD_mesh%fluid, MHD_mesh%conduct, iphys, nod_fld, jacs, &
     &    fem_sq%i_rms, fem_sq%j_ave, fem_sq%i_msq, fem_sq%msq_list,    &
     &    rhs_mat%fem_wk, mhd_fem_wk, fem_sq%msq)
      call int_no_evo_mean_squares(time_d%i_time_step, time_d%dt,       &
     &    mesh, MHD_prop%fl_prop, MHD_prop%cd_prop,                     &
     &    iphys, nod_fld, iphys_ele, ele_fld, MHD_mesh%fluid,           &
     &    jacs, fem_sq%i_rms, fem_sq%j_ave, rhs_mat%fem_wk, fem_sq%msq)
!
      call MPI_allREDUCE(fem_sq%msq%ave_local, fem_sq%msq%ave_global,   &
     &    fem_sq%msq%num_ave, CALYPSO_REAL, MPI_SUM,                    &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE(fem_sq%msq%rms_local, fem_sq%msq%rms_global,   &
     &    fem_sq%msq%num_rms, CALYPSO_REAL, MPI_SUM,                    &
     &    CALYPSO_COMM, ierr_MPI)
!
!
       do nd = 1, fem_sq%msq%num_ave
         fem_sq%msq%ave_global(nd) = fem_sq%msq%ave_global(nd)          &
     &                   / fem_sq%msq%rms_global(fem_sq%i_msq%ivol)
       end do
       do nd = 1, fem_sq%msq%num_rms - 1
           if (nd .eq. fem_sq%i_rms%i_velo                              &
     &    .or. nd .eq. fem_sq%i_rms%i_magne                             &
     &    .or. nd .eq. fem_sq%i_msq%ir_me_ic                            &
     &    .or. nd .eq. fem_sq%i_rms%i_vort                              &
     &    .or. nd .eq. fem_sq%i_rms%i_current                           &
     &    .or. nd .eq. fem_sq%i_msq%ir_sqj_ic                           &
     &    .or. nd .eq. fem_sq%i_rms%i_filter_velo                       &
     &    .or. nd .eq. fem_sq%i_rms%i_filter_magne                      &
     &    .or. nd .eq. fem_sq%i_msq%ir_me_f_ic) then
            fem_sq%msq%rms_global(nd) = fem_sq%msq%rms_global(nd)       &
     &                    / fem_sq%msq%rms_global(fem_sq%i_msq%ivol)
        else
          fem_sq%msq%rms_global(nd) = sqrt(fem_sq%msq%rms_global(nd)    &
     &                    / fem_sq%msq%rms_global(fem_sq%i_msq%ivol))
        end if
      end do
!
      call output_monitor_file(my_rank, time_d%i_time_step,             &
     &    time_d%time, iphys, fem_sq%msq, fem_sq%msq_list)
!
      end subroutine output_time_step_control
!
!  ---------------------------------------------------------------------
!
      end module t_FEM_MHD_mean_square

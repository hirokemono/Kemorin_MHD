!FEM_flexible_time_step.f90
!     module FEM_flexible_time_step
!
!      Written by H. Matsui on Nov., 2009
!
!!      subroutine set_new_time_and_step                                &
!!     &         (cd_prop, iphys, nod_fld, time_d)
!!      subroutine s_check_flexible_time_step                           &
!!     &         (mesh, MHD_mesh, cd_prop, iphys, nod_fld, jacs,        &
!!     &          rhs_mat, flex_data, flex_p, time_d)
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(time_data), intent(inout) :: time_d
!
      module FEM_flexible_time_step
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
!
      use t_flex_delta_t_data
!
      implicit  none
!
      integer(kind=kint), parameter :: dt_check_max_code =   15
      integer(kind=kint), parameter :: dt_check_min_code =   17
!
      character(len=kchara), parameter                                  &
     &      :: dt_check_max_name = 'maximum_dt_chack.dat'
      character(len=kchara), parameter                                  &
     &      :: dt_check_min_name = 'minimum_dt_chack.dat'
!
!
      private :: check_flex_time_step_by_sq
!
      private :: dt_check_max_code, dt_check_min_code
      private :: dt_check_max_name, dt_check_min_name
      private :: shrink_delta_t, extend_delta_t
      private :: open_flex_step_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_check_flexible_time_step                             &
     &         (mesh, MHD_mesh, MHD_prop, iphys, nod_fld, fem_int,      &
     &          rhs_mat, flex_MHD, MHD_step)
!
      use t_mesh_data
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_control_parameter
      use t_work_fem_integration
      use t_FEM_MHD_time_stepping
      use t_mhd_step_parameter
!
      use check_deltat_by_prev_rms
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(finite_element_integration), intent(in) :: fem_int
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
      type(MHD_step_param), intent(inout) :: MHD_step
!
!
      if(MHD_step%flex_p%iflag_flexible_step .ne. iflag_flex_step)      &
     &    return
!
      if (iflag_debug.eq.1) write(*,*) 'check_flex_time_step_by_sq'
      call check_flex_time_step_by_sq(mesh, MHD_mesh,                   &
     &    MHD_prop%cd_prop, iphys, nod_fld, fem_int%jcs, rhs_mat,       &
     &    flex_MHD%flex_data, MHD_step%flex_p, MHD_step%time_d)
!
      end subroutine s_check_flexible_time_step
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_new_time_and_step                                  &
     &         (cd_prop, iphys, nod_fld, flex_p, time_d)
!
      use t_time_data
      use t_material_property
      use t_phys_data
      use t_phys_address
!
      use copy_field_data_4_dt_check
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
!
      type(time_data), intent(inout) :: time_d
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(phys_data), intent(inout) :: nod_fld
!
!
      time_d%time = time_d%time + time_d%dt
      time_d%i_time_step = time_d%i_time_step + 1
!
      if (flex_p%iflag_flexible_step .eq. iflag_fixed_step) then
        flex_p%istep_max_dt = time_d%i_time_step
      else
        flex_p%istep_flex_to_max = flex_p%istep_flex_to_max + 1
        flex_p%istep_flex_to_max                                        &
     &     = mod(flex_p%istep_flex_to_max,flex_p%interval_flex_2_max)
        if(flex_p%istep_flex_to_max .eq. 0) then
          flex_p%istep_max_dt = flex_p%istep_max_dt + 1
        end if
      end if
!
      if (iflag_debug.eq.1) write(*,*) 's_copy_field_data_for_dt_check'
      call s_copy_field_data_for_dt_check(cd_prop, iphys, nod_fld)
!
      end subroutine set_new_time_and_step
!
! -----------------------------------------------------------------------
!
      subroutine check_flex_time_step_by_sq                             &
     &         (mesh, MHD_mesh, cd_prop, iphys, nod_fld, jacs,          &
     &          rhs_mat, flex_data, flex_p, time_d)
!
      use t_mesh_data
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_flex_delta_t_data
      use t_physical_property
      use t_work_FEM_integration
!
      use check_deltat_by_prev_rms
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(flexible_stepping_data), intent(inout) :: flex_data
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(time_data), intent(inout) :: time_d
!
!
      if( mod(flex_p%istep_flex_to_max,itwo) .eq. izero) then
!        call s_check_deltat_by_previous                                &
!     &     (mesh%node, MHD_prop1%cd_prop, iphys, nod_fld, flex_data)
        call check_difference_by_prev_rms                               &
     &     (time_d%time, mesh, MHD_mesh%fluid, cd_prop, iphys, nod_fld, &
     &      jacs, rhs_mat%fem_wk, flex_data)
!
        if(flex_data%d_ratio_allmax .gt. flex_p%min_eps_to_expand)      &
     &   then
          call shrink_delta_t                                           &
     &       (time_d%i_time_step, flex_data, flex_p%dt_fact,            &
     &        flex_p%idt_digit, flex_p%istep_flex_to_max,               &
     &        flex_p%interval_flex_2_max, time_d%dt)
          flex_p%iflag_flex_step_changed = id_turn_ON
          return
        else
          flex_p%iflag_flex_step_changed = id_turn_OFF
        end if
!
        if(flex_p%istep_flex_to_max .eq. 0) then
          if(flex_data%d_ratio_allmax .lt. flex_p%max_eps_to_shrink)    &
     &     then
            call extend_delta_t                                         &
     &         (time_d%i_time_step, flex_data, flex_p%dt_fact,          &
     &          flex_p%idt_digit, flex_p%istep_flex_to_max,             &
     &          flex_p%interval_flex_2_max, time_d%dt)
            flex_p%iflag_flex_step_changed = id_turn_ON
          end if
!
          if(my_rank .eq. izero) then
            call open_flex_step_monitor(flex_data)
            call write_rms_delta_t_check(dt_check_max_code,             &
     &          time_d%i_time_step, time_d%time, flex_data)
            close(dt_check_max_code)
          end if
        end if
      end if
!
      end subroutine check_flex_time_step_by_sq
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine shrink_delta_t(i_step, flex_data, dt_fact, idt_digit,  &
     &          istep_flex_to_max, interval_flex_2_max, dt)
!
      type(flexible_stepping_data), intent(in) :: flex_data
!
      integer(kind=kint), intent(in) :: i_step
      real(kind=kreal), intent(inout) :: dt_fact
      integer(kind = kint), intent(inout) :: idt_digit
      integer(kind = kint), intent(inout) :: istep_flex_to_max
      integer(kind = kint), intent(inout) :: interval_flex_2_max
      real(kind = kreal), intent(inout) :: dt
!
!
      if(my_rank .eq. izero) then
        write(*,*) 'Shrink Delta t from ', dt, ' at ', i_step,          &
     &            'd_ratio_max', flex_data%d_ratio_allmax
      end if
      if(iflag_debug .gt. izero) then
        write(*,*) 'Old temporal step is ', istep_flex_to_max,          &
     &             'to ', istep_flex_to_max
      end if
!
      if     (dt_fact .eq. one) then
        dt_fact =   five
        idt_digit = idt_digit - 1
        interval_flex_2_max = interval_flex_2_max * 2
        istep_flex_to_max = istep_flex_to_max * 2
      else if(dt_fact .eq. two) then
        dt_fact = one
        interval_flex_2_max = (interval_flex_2_max * 5) / 2
        istep_flex_to_max = (istep_flex_to_max * 5) / 2
      else if(dt_fact .eq. five) then
        dt_fact = two
        interval_flex_2_max = interval_flex_2_max * 2
        istep_flex_to_max = istep_flex_to_max * 2
      end if
!
      dt = dt_fact * ten**(idt_digit)
!
      if(my_rank .eq. izero) then
        write(*,*) 'New Delta t is ', dt
      end if
      if(iflag_debug .gt. izero) then
        write(*,*) 'New temporal step is ', istep_flex_to_max,          &
     &             'to ', istep_flex_to_max
      end if
!
      end subroutine shrink_delta_t
!
! -----------------------------------------------------------------------
!
      subroutine extend_delta_t(i_step, flex_data, dt_fact, idt_digit,  &
     &          istep_flex_to_max, interval_flex_2_max, dt)
!
      type(flexible_stepping_data), intent(in) :: flex_data
!
      integer(kind=kint), intent(in) :: i_step
      real(kind=kreal), intent(inout) :: dt_fact
      integer(kind = kint), intent(inout) :: idt_digit
      integer(kind = kint), intent(inout) :: istep_flex_to_max
      integer(kind = kint), intent(inout) :: interval_flex_2_max
      real(kind = kreal), intent(inout) :: dt
!
!
      if(my_rank .eq. izero) then
        write(*,*) 'Extend Delta t from ', dt, ' at ', i_step,          &
     &            'd_ratio_max', flex_data%d_ratio_allmax
      end if
      if(iflag_debug .gt. izero) then
        write(*,*) 'Old temporal step is ', istep_flex_to_max,          &
     &             'to ', istep_flex_to_max
      end if
!
      if     (dt_fact .eq. one) then
        dt_fact =   two
        interval_flex_2_max = interval_flex_2_max / 2
        istep_flex_to_max =     istep_flex_to_max / 2
      else if(dt_fact .eq. two) then
        dt_fact = five
        interval_flex_2_max = (interval_flex_2_max * 2) / 5
        istep_flex_to_max =     (istep_flex_to_max * 2) / 5
      else if(dt_fact .eq. five) then
        dt_fact = one
        idt_digit = idt_digit + 1
        interval_flex_2_max = interval_flex_2_max / 2
        istep_flex_to_max =     istep_flex_to_max / 2
      end if
!
      dt = dt_fact * ten**(idt_digit)
!
      if(my_rank .eq. izero) then
        write(*,*) 'New Delta t is ', dt
      end if
      if(iflag_debug .gt. izero) then
        write(*,*) 'New temporal step is ', istep_flex_to_max,          &
     &             'to ', istep_flex_to_max
      end if
!
      end subroutine extend_delta_t
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_flex_step_monitor(flex_data)
!
      type(flexible_stepping_data), intent(inout) :: flex_data
!
!
      open(dt_check_max_code, file=dt_check_max_name,                   &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
  99  continue
      open(dt_check_max_code, file = dt_check_max_name)
      call write_delta_t_check_head(dt_check_max_code, flex_data)
!
      end subroutine open_flex_step_monitor
!
! -----------------------------------------------------------------------
!
      end module FEM_flexible_time_step

!
!      module read_boundary_condition_file
!
!      Written by H. Matsui on July, 2005
!
!      subroutine read_boundary_files
!
      module read_boundary_condition_file
!
      use m_precision
!
      implicit none
!
      private :: serch_boundary_file_scalar
      private :: serch_boundary_file_vector
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_boundary_files
!
      use m_parallel_var_dof
      use m_control_parameter
      use m_bc_data_list
      use m_surf_data_list
!
      use read_bc_values_file_1st
!
      integer (kind=kint) :: iflag
!
!
      iflag = 0
!
! ----  read boundary data for temperature
!
      if ( iflag_t_evo_4_temp .ge. 1 ) then
!
       call serch_boundary_file_scalar(iflag, num_bc_e, ibc_e_type)
       call serch_boundary_file_scalar(iflag, num_bc_h_flux,            &
     &       ibc_h_flux_type)
!
      end if
!
! ----  read boundary data for velocity
!
      if ( iflag_t_evo_4_velo .ge. 1 ) then
!
       call serch_boundary_file_vector(iflag, num_bc_v, ibc_v_type)
       call serch_boundary_file_vector(iflag, num_bc_tq, ibc_tq_type)
!
!  set boundary conditions for pressure
!
       call serch_boundary_file_scalar(iflag, num_bc_p, ibc_p_type)
!
      end if
!
! ----  read boundary data for dummy scalar
!
      if ( iflag_t_evo_4_composit .ge. 1 ) then
!
       call serch_boundary_file_scalar(iflag, num_bc_composit,          &
     &      ibc_composit_type)
!
      end if
!
! ----  read boundary data for magnetic field
!
      if ( iflag_t_evo_4_magne .ge. 1                                   &
     &      .or. iflag_t_evo_4_vect_p .ge. 1) then
!
       call serch_boundary_file_vector(iflag, num_bc_b, ibc_b_type)
       call serch_boundary_file_vector(iflag, num_bc_bs, ibc_bs_type)
!
! ----  read boundary data for magnetic potential
!
       call serch_boundary_file_scalar(iflag, num_bc_mag_p,             &
     &       ibc_mag_p_type)
!
      end if
!
! ----  read boundary data for vector potential
!
      if ( iflag_t_evo_4_vect_p .ge. 1 ) then
!
       call serch_boundary_file_vector(iflag, num_bc_vp, ibc_vp_type)
       call serch_boundary_file_vector(iflag, num_bc_vps, ibc_vps_type)
!
! ----  read boundary data for magnetic potential
!
       call serch_boundary_file_scalar(iflag, num_bc_mag_p,             &
     &       ibc_mag_p_type)
!
      end if
!
! ----  read boundary data for current density
!
      if ( iflag_t_evo_4_magne .ge. 1                                   &
     &      .or. iflag_t_evo_4_vect_p .ge. 1) then
!
       call serch_boundary_file_vector(iflag, num_bc_j, ibc_j_type)
       call serch_boundary_file_vector(iflag, num_bc_js, ibc_js_type)
!
      end if
!
! ----  open data file for boundary data
!
      if ( iflag .eq. 1 ) call read_boundary_values_file_1(my_rank)
!
      end subroutine read_boundary_files
!
! -----------------------------------------------------------------------
!
       subroutine serch_boundary_file_scalar( iflag, num_bc, ibc_type)
!
       integer (kind = kint), intent(inout) :: iflag
       integer (kind = kint), intent(in) :: num_bc
       integer (kind = kint), intent(in) :: ibc_type(num_bc)
!
       integer(kind = kint) :: i
!
       if (iflag .eq. 0 ) then
        if (num_bc .gt. 0) then
         do i = 1, num_bc
          if ( ibc_type(i).eq.-1 .or. ibc_type(i).eq.20 ) then
           iflag = 1
          end if
         end do
        end if
       end if
!
       end subroutine serch_boundary_file_scalar
!
! -----------------------------------------------------------------------
!
       subroutine serch_boundary_file_vector( iflag, num_bc, ibc_type)
!
       integer (kind = kint), intent(inout) :: iflag
       integer (kind = kint), intent(in) :: num_bc
       integer (kind = kint), intent(in) :: ibc_type(num_bc)
!
       integer(kind = kint) :: i
!
       if (iflag .eq. 0 ) then
        if (num_bc .gt. 0) then
         do i = 1, num_bc
          if (   ibc_type(i).eq.-1 .or. ibc_type(i).eq.-2               &
     &      .or. ibc_type(i).eq.-3 .or. ibc_type(i).eq.21               &
     &      .or. ibc_type(i).eq.22 .or. ibc_type(i).eq.23) then
          iflag = 1
          end if
         end do
        end if
       end if
!
       end subroutine serch_boundary_file_vector
!
! -----------------------------------------------------------------------
!
      end module read_boundary_condition_file

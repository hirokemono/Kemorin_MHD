!> @file  t_sph_boundary_input_data.f90
!!      module t_sph_boundary_input_data
!!
!! @author  H. Matsui
!! @date Programmed in Dec. 2012
!
!> @brief Boundary condition data from external file
!!
!!@verbatim
!!      subroutine dealloc_sph_bc_item_ctl(bc_IO)
!!        type(boundary_spectra), intent(inout) :: bc_IO
!!
!!      subroutine read_boundary_spectr_file(bc_IO)
!!        type(boundary_spectra), intent(inout) :: bc_IO
!!      subroutine write_boundary_spectr_file(bc_IO)
!!        type(boundary_spectra), intent(in) :: bc_IO
!!
!!      subroutine set_fixed_vector_bc_by_file(field_name, sph_rj,      &
!!     &          bc_IO, ref_grp, iflag_bc_vector, bc_Vspec, bc_Vevo)
!!      subroutine set_fixed_scalar_bc_by_file(field_name, sph_rj,      &
!!     &          bc_IO, ref_grp, iflag_bc_scalar, bc_Sspec, bc_Sevo)
!!      subroutine set_fixed_gradient_bc_by_file(field_name, sph_rj,    &
!!     &          bc_IO, ref_grp, iflag_bc_scalar, bc_Sspec, bc_Sevo)
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_vector_BC_coef), intent(inout) :: bc_Vspec
!!        type(sph_vector_BC_evo), intent(inout) :: bc_Vevo
!!        type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
!!        type(sph_scalar_BC_evo), intent(inout) :: bc_Sevo
!!
!!
!!  ---------------------------------------------------------------------
!!      Data format
!!       line 1:  Number of total boundary conditions to be defined
!!
!!       line 2:     Field name to define the first boundary condition
!!       line 3:     Place to define the first boundary condition
!!       line 4:     Number of spherical harmonics modes 
!!                    for each boundary condition
!!       line 5...:  Spectrum data for the boundary conditions 
!!                  (degree $l$, order $m$, and harmonics coefficients)
!!        Return to 2...
!!  ---------------------------------------------------------------------
!!@endverbatim
!!
!!@param    field_name       Field name to be define
!!                           the boundary condition by file
!!@param    ref_grp          Boundary group name to be defined
!!                           the boundary condition by file
!!@param    bc_data(sph_rj%nidx_rj(2)) Local boundary condition spectrum
!!@param    iflag_bc_scalar  Boundary condition type flag
!
      module t_sph_boundary_input_data
!
      use m_precision
      use t_spheric_rj_data
      use t_each_sph_boundary_IO_data
!
      implicit  none
!
!>      File ID for boundary condition file
      integer(kind = kint), parameter :: id_boundary_file = 41
!
      type boundary_spectra
!>        File name for boundary condition file
        character(len=kchara) :: file_name = 'boundary_spectr.dat'
!>        Name of boundary conditions to set
        integer(kind = kint) :: num_bc_fld
!>        Structures for boundary conditions
        type(each_boundary_spectr), allocatable :: ctls(:)
      end type boundary_spectra
!
!
      private :: id_boundary_file
      private :: alloc_sph_bc_item_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_bc_item_ctl(bc_IO)
!
      type(boundary_spectra), intent(inout) :: bc_IO
!
!
      allocate(bc_IO%ctls(bc_IO%num_bc_fld))
      bc_IO%ctls(1:bc_IO%num_bc_fld)%num_bc_mode = 0
      bc_IO%ctls(1:bc_IO%num_bc_fld)%ncomp_bc =    1
!
      end subroutine alloc_sph_bc_item_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_bc_item_ctl(bc_IO)
!
      type(boundary_spectra), intent(inout) :: bc_IO
!
      integer(kind = kint) :: i
!
!
      do i = 1, bc_IO%num_bc_fld
        call dealloc_each_bc_item_ctl(bc_IO%ctls(i))
      end do
      deallocate(bc_IO%ctls)
!
      end subroutine dealloc_sph_bc_item_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_boundary_spectr_file(bc_IO)
!
      use m_machine_parameter
      use skip_comment_f
!
      type(boundary_spectra), intent(inout) :: bc_IO
!
      integer(kind = kint) :: igrp
      character(len=255) :: tmpchara
!
!
      if(iflag_debug .gt. 0) write(*,*) 'read boundary condition: ',    &
     &                      trim(bc_IO%file_name)
      open(id_boundary_file, file=bc_IO%file_name)
!
      call skip_comment(tmpchara,id_boundary_file)
      read(tmpchara,*) bc_IO%num_bc_fld
!
      call alloc_sph_bc_item_ctl(bc_IO)
!
      do igrp = 1, bc_IO%num_bc_fld
        call read_each_boundary_spectr                                  &
     &     (id_boundary_file, bc_IO%ctls(igrp))
      end do
      close(id_boundary_file)
!
      end subroutine read_boundary_spectr_file
!
! -----------------------------------------------------------------------
!
      subroutine write_boundary_spectr_file(bc_IO)
!
      type(boundary_spectra), intent(in) :: bc_IO
!
      integer(kind = kint) :: igrp
!
!
      open(id_boundary_file, file=bc_IO%file_name)
!
      write(id_boundary_file,'(a)') '#'
      write(id_boundary_file,'(a)') '#  number of boundary conditions'
      write(id_boundary_file,'(a)') '#'
!
      write(id_boundary_file,'(i16)') bc_IO%num_bc_fld
!
      do igrp = 1, bc_IO%num_bc_fld
        call write_each_boundary_spectr                                 &
     &     (id_boundary_file, bc_IO%ctls(igrp))
      end do
!
      close(id_boundary_file)
!
      end subroutine write_boundary_spectr_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_fixed_vector_bc_by_file(field_name, sph_rj,        &
     &          bc_IO, ref_grp, iflag_bc_vector, bc_Vspec, bc_Vevo)
!
      use skip_comment_f
      use set_sph_boundary_from_file
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_rj_grid), intent(in) :: sph_rj
!
      character(len=kchara), intent(in) :: field_name
      character(len=kchara), intent(in) :: ref_grp
!
      integer(kind = kint), intent(inout) :: iflag_bc_vector
      type(sph_vector_BC_coef), intent(inout) :: bc_Vspec
      type(sph_vector_BC_evo), intent(inout) :: bc_Vevo
!
      integer(kind = kint) :: igrp
!
!
      do igrp = 1, bc_IO%num_bc_fld
        if(cmp_no_case(bc_IO%ctls(igrp)%bc_group, ref_grp)) then
          if(cmp_no_case(bc_IO%ctls(igrp)%bc_field, field_name)) then
            iflag_bc_vector =  iflag_fixed_field
            call set_bc_4_sph_vector_by_file(sph_rj, bc_IO%ctls(igrp),  &
     &          bc_Vspec%Vp_BC, bc_Vspec%Dp_BC, bc_Vspec%Vt_BC)
!
          else if(find_bc_label(bc_IO%ctls(igrp)%bc_field,              &
     &                          field_name, mag_label)) then
            iflag_bc_vector =  iflag_evolve_field
            call bc_4_evo_vector_sph_by_file(sph_rj, bc_IO%ctls(igrp),  &
     &         bc_Vevo%Vp_BC_mag, bc_Vevo%Dp_BC_mag, bc_Vevo%Vt_BC_mag)
          else if(find_bc_label(bc_IO%ctls(igrp)%bc_field,              &
     &                          field_name, freq1_label)                &
     &       .or. find_bc_label(bc_IO%ctls(igrp)%bc_field,              &
     &                          field_name, freq2_label)) then
            iflag_bc_vector =  iflag_evolve_field
            call bc_4_evo_vect2_sph_by_file(sph_rj, bc_IO%ctls(igrp),   &
     &          bc_Vevo%Vp_BC_freq, bc_Vevo%Vt_BC_freq)
          else if(find_bc_label(bc_IO%ctls(igrp)%bc_field,              &
     &                        field_name, phase_label)) then
            iflag_bc_vector =  iflag_evolve_field
            call bc_4_evo_vector_sph_by_file(sph_rj, bc_IO%ctls(igrp),  &
     &          bc_Vevo%Vp_BC_phase, bc_Vevo%Dp_BC_phase,               &
     &          bc_Vevo%Vt_BC_phase)
          end if
        end if
      end do
!
      end subroutine set_fixed_vector_bc_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_scalar_bc_by_file(field_name, sph_rj,        &
     &          bc_IO, ref_grp, iflag_bc_scalar, bc_Sspec, bc_Sevo)
!
      use skip_comment_f
      use set_sph_boundary_from_file
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_rj_grid), intent(in) :: sph_rj
!
      character(len=kchara), intent(in) :: field_name
      character(len=kchara), intent(in) :: ref_grp
!
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
      type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
      type(sph_scalar_BC_evo), intent(inout) :: bc_Sevo
!
      integer(kind = kint) :: igrp
!
!
      do igrp = 1, bc_IO%num_bc_fld
        if(cmp_no_case(bc_IO%ctls(igrp)%bc_group, ref_grp)) then
          if(cmp_no_case(bc_IO%ctls(igrp)%bc_field, field_name)) then
            iflag_bc_scalar =  iflag_fixed_field
            call set_bc_4_sph_scalar_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sspec%S_BC)
!
          else if(find_bc_label(bc_IO%ctls(igrp)%bc_field,              &
     &                          field_name, mag_label)) then
            iflag_bc_scalar =  iflag_evolve_field
            call bc_4_evo_scalar_sph_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sevo%S_BC_mag)
          else if(find_bc_label(bc_IO%ctls(igrp)%bc_field,              &
     &                          field_name, freq1_label)                &
     &       .or. find_bc_label(bc_IO%ctls(igrp)%bc_field,              &
     &                          field_name, freq2_label)) then
            iflag_bc_scalar =  iflag_evolve_field
            call bc_4_evo_scalar_sph_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sevo%S_BC_freq)
          else if(find_bc_label(bc_IO%ctls(igrp)%bc_field,              &
     &                          field_name, phase_label)) then
            iflag_bc_scalar =  iflag_evolve_field
            call bc_4_evo_scalar_sph_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sevo%S_BC_phase)
          end if
        end if
      end do
!
      end subroutine set_fixed_scalar_bc_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_gradient_bc_by_file(field_name, sph_rj,      &
     &          bc_IO, ref_grp, iflag_bc_scalar, bc_Sspec, bc_Sevo)
!
      use skip_comment_f
      use set_sph_boundary_from_file
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_rj_grid), intent(in) :: sph_rj
!
      character(len=kchara), intent(in) :: field_name
      character(len=kchara), intent(in) :: ref_grp
!
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
      type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
      type(sph_scalar_BC_evo), intent(inout) :: bc_Sevo
!
      integer(kind = kint) :: igrp
!
!
      do igrp = 1, bc_IO%num_bc_fld
        if(cmp_no_case(bc_IO%ctls(igrp)%bc_group, ref_grp)) then
          if(cmp_no_case(bc_IO%ctls(igrp)%bc_field, field_name)) then
            iflag_bc_scalar =  iflag_fixed_flux
            call set_bc_4_sph_scalar_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sspec%S_BC)
!
          else if(find_bc_label(bc_IO%ctls(igrp)%bc_field,              &
     &                          field_name, mag_label)) then
            iflag_bc_scalar =  iflag_evolve_flux
            call bc_4_evo_scalar_sph_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sevo%S_BC_mag)
          else if(find_bc_label(bc_IO%ctls(igrp)%bc_field,              &
     &                          field_name, freq1_label)                &
     &       .or. find_bc_label(bc_IO%ctls(igrp)%bc_field,              &
     &                          field_name, freq2_label)) then
            iflag_bc_scalar =  iflag_evolve_flux
            call bc_4_evo_scalar_sph_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sevo%S_BC_freq)
          else if(find_bc_label(bc_IO%ctls(igrp)%bc_field,              &
     &                          field_name, phase_label)) then
            iflag_bc_scalar =  iflag_evolve_flux
            call bc_4_evo_scalar_sph_by_file                            &
     &           (sph_rj, bc_IO%ctls(igrp), BC_Sevo%S_BC_phase)
          end if
        end if
      end do
!
      end subroutine set_fixed_gradient_bc_by_file
!
! -----------------------------------------------------------------------
!
      end module t_sph_boundary_input_data

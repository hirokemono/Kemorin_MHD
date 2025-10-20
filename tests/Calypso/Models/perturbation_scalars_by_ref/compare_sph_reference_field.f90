!>@file   compare_sph_reference_field.f90
!!@brief  program compare_sph_reference_field
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program to compare reference field output
      program compare_sph_reference_field
!
      use m_precision
      use m_constants
!
      use calypso_mpi
!
      implicit none
!
      character(len = kchara) :: ref_name, file_name
!
!
      if(command_argument_count() .lt. 2) then
        write(*,*) 'Command [REFERENCE FILE NAME] [COMPARED FILE NAME]'
        stop
      end if
      call get_command_argument(1, ref_name)
      call get_command_argument(2, file_name)
!
      call calypso_MPI_init
      call analyze_compare_sph_reference(ref_name, file_name)
      call calypso_MPI_finalize
      stop '***** program finished *****'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine analyze_compare_sph_reference(file1_name, file2_name)
!
      use calypso_mpi_int
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_data
      use t_time_data
      use t_field_data_IO
      use field_file_IO
      use copy_rj_phys_data_4_IO
      use compare_by_assemble_sph
!
      implicit none
!
      character(len = kchara), intent(in) :: file1_name, file2_name
!
      type(sph_grids) :: sph1
      type(phys_data) :: r_fld1
      type(time_data) :: t1_IO, t2_IO
      type(field_IO) :: fld1_IO, fld2_IO
      integer(kind = kint) :: i, iend
      integer(kind = kint) :: iflag, iflag_gl
      character(len=kchara) :: charaint
!
!
      call read_and_alloc_step_field                                    &
     &   (file1_name, izero, t1_IO, fld1_IO, iend)
!
      sph1%sph_rj%nnod_rj = fld1_IO%nnod_IO
      call alloc_spheric_param_rj(sph1%sph_rj)
!$omp parallel do
      do i = 1, sph1%sph_rj%nnod_rj
        sph1%sph_rj%idx_global_rj(i,1) = i
        sph1%sph_rj%idx_global_rj(i,2) = 0
      end do
!$omp end parallel do
!
      call copy_rj_phys_name_from_IO(fld1_IO, r_fld1)
      call alloc_phys_data(fld1_IO%nnod_IO, r_fld1)
      call copy_rj_phys_data_from_IO(fld1_IO, r_fld1)
!
!
      call read_and_alloc_step_field                                    &
     &   (file2_name, izero, t2_IO, fld2_IO, iend)
!
      iflag = compare_assembled_sph_data(TINY, t1_IO, sph1, r_fld1,     &
     &                                   fld2_IO, t2_IO)
!
      call calypso_mpi_allreduce_one_int(iflag, iflag_gl, MPI_MAX)
!
      if(iflag_gl.gt.0) then
        write(e_message,'(a)') 'Data do not have consistentency'
        call calypso_mpi_abort(1,e_message)
      else
        write(*,*) 'Data have a consistecy.'
      end if
!
      open(999,file='flag.txt')
      write(charaint,*) iflag_gl
      write(999,'(a)') trim(ADJUSTL(charaint))
      close(999)
!
      end subroutine analyze_compare_sph_reference
!
! ----------------------------------------------------------------------
!
      end program compare_sph_reference_field

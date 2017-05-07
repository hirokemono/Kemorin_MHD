!>@file   m_maxmode_sph_ene_spectr.f90
!!        module m_maxmode_sph_ene_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine sph_maximum_pwr_spectr                               &
!!     &         (fname_org, start_time, end_time, sph_IN)
!!@endverbatim
!
      module m_maxmode_sph_ene_spectr
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
!
      implicit none
!
!
      integer(kind = kint), parameter :: id_file_rms_l =    35
      integer(kind = kint), parameter :: id_file_maxval =   44
      integer(kind = kint), parameter :: id_file_maxloc =   45
!
      type(read_sph_spectr_data), save :: sph_OUT1
!
      real(kind = kreal), allocatable :: max_spectr(:,:,:)
      real(kind = kreal), allocatable :: max_degree(:,:,:)
!
!
      private :: sph_OUT1, max_spectr, max_degree
      private :: id_file_rms_l, id_file_maxval, id_file_maxloc
      private :: find_dominant_scale_sph
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sph_maximum_pwr_spectr                                 &
     &         (fname_org, start_time, end_time, sph_IN)
!
      use sph_mean_square_IO_select
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(in) :: start_time, end_time
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      character(len = kchara) :: file_name
      integer(kind = kint) :: i, icou, ierr, ist_true
!
!
      sph_IN%iflag_spectr = 1
      open(id_file_rms_l, file=fname_org)
      call select_input_sph_pwr_head(id_file_rms_l, sph_IN)
!
      sph_OUT1%iflag_spectr =  0
      call copy_read_ene_params_4_sum(sph_IN, sph_OUT1)
!
      write(file_name, '(a7,a)') 'maxval_', trim(fname_org)
      open(id_file_maxval, file=file_name)
      call select_output_sph_pwr_head(id_file_maxval, sph_OUT1)
!
      write(file_name, '(a7,a)') 'maxloc_', trim(fname_org)
      open(id_file_maxloc, file=file_name)
      call select_output_sph_pwr_head(id_file_maxloc, sph_OUT1)
!
      call allocate_max_sph_data(sph_IN)
!
      icou = 0
      ist_true = -1
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=  ', icou
      do
        ierr = select_input_sph_pwr_data(id_file_rms_l, sph_IN)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. start_time) then
          call copy_read_ene_step_data(sph_IN, sph_OUT1)
          call find_dominant_scale_sph                                  &
     &       (sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,     &
     &        sph_IN%spectr_IO, max_spectr, max_degree)
          icou = icou + 1
!
          call copy_ene_spectr_data_to_IO                               &
     &       (sph_IN%nri_sph, izero, sph_IN%ntot_sph_spec,              &
     &        max_spectr, sph_OUT1)
          call select_output_sph_pwr_data(id_file_maxval, sph_OUT1)
!
          call copy_ene_spectr_data_to_IO                               &
     &       (sph_IN%nri_sph, izero, sph_IN%ntot_sph_spec,              &
     &        max_degree, sph_OUT1)
          call select_output_sph_pwr_data(id_file_maxloc, sph_OUT1)
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=   ', icou
        if (sph_IN%time .ge. end_time) exit
      end do
!
   99 continue
      write(*,*)
      close(id_file_rms_l)
      close(id_file_maxval)
      close(id_file_maxloc)
!
!
      call deallocate_max_sph_data
      call dealloc_sph_espec_data(sph_IN)
      call dealloc_sph_espec_data(sph_OUT1)
!
      end subroutine sph_maximum_pwr_spectr
!
!   --------------------------------------------------------------------
!
      subroutine find_dominant_scale_sph(nri_sph, ltr_sph, ncomp,       &
     &          spectr_l, max_data, max_mode)
!
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout) :: max_data(ncomp,0:0,nri_sph)
      real(kind = kreal), intent(inout) :: max_mode(ncomp,0:0,nri_sph)
!
      integer(kind = kint) :: kr, nd, lth
!
!
!$omp parallel private(kr,lth,nd)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp
          max_data(nd,0,kr) = spectr_l(nd,0,kr)
          max_mode(nd,0,kr) = 0
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      do lth = 1, ltr_sph
!$omp parallel private(kr,nd)
        do kr = 1, nri_sph
!$omp do
          do nd = 1, ncomp
            if(spectr_l(nd,lth,kr) .gt. max_data(nd,0,kr)) then
              max_data(nd,0,kr) = spectr_l(nd,lth,kr)
              max_mode(nd,0,kr) = lth
            end if
          end do
!$omp end do nowait
        end do
!$omp end parallel
      end do
!
      end subroutine find_dominant_scale_sph
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine allocate_max_sph_data(sph_IN)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      allocate( max_spectr(sph_IN%ntot_sph_spec,0:0,sph_IN%nri_sph) )
      allocate( max_degree(sph_IN%ntot_sph_spec,0:0,sph_IN%nri_sph) )
      max_spectr = 0.0d0
      max_degree = 0.0d0
!
      end subroutine allocate_max_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_max_sph_data
!
      deallocate(max_spectr, max_degree)
!
      end subroutine deallocate_max_sph_data
!
!   --------------------------------------------------------------------
!
      end module m_maxmode_sph_ene_spectr

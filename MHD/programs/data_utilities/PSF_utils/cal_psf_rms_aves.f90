!
!      module cal_psf_rms_aves
!
      module cal_psf_rms_aves
!
!      Written by H. Matsui on Apr., 2005
!      Modified by H. Matsui on Feb., 2009
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), parameter :: id_ave_psf = 21
      integer(kind = kint), parameter :: id_rms_psf = 22
      character(len=kchara), parameter :: fname_ave_psf = 'psf_ave.dat'
      character(len=kchara), parameter :: fname_rms_psf = 'psf_rms.dat'
!
      private :: id_ave_psf, fname_ave_psf
      private :: id_rms_psf, fname_rms_psf
!
!      subroutine open_psf_ave_rms_data
!      subroutine s_cal_psf_rms_aves
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine open_psf_ave_rms_data
!
      use m_norms_4_psf
      use take_normals_4_psf
!
!
      call allocate_norms_4_psf
      call cal_norm_area_4_psf
!
      open(id_ave_psf, file=fname_ave_psf, form='formatted',            &
     &     status='old',err = 100, position='APPEND')
      open(id_rms_psf, file=fname_rms_psf, form='formatted',            &
     &     status='old',err = 100, position='APPEND')
!
      return
!
  100 continue
!
      close(id_ave_psf)
      close(id_rms_psf)
!
      open(id_ave_psf, file=fname_ave_psf, form='formatted')
      open(id_rms_psf, file=fname_rms_psf, form='formatted')
!
      call write_headers_psf_comp_name(id_ave_psf)
      write(id_ave_psf,*) 'area_size, '
!
      call write_headers_psf_comp_name(id_rms_psf)
      write(id_rms_psf,*)
!
      end subroutine open_psf_ave_rms_data
!
!-----------------------------------------------------------------------
!
      subroutine close_psf_ave_rms_data
!
!
      close(id_ave_psf)
      close(id_rms_psf)
!
      end subroutine close_psf_ave_rms_data
!
!-----------------------------------------------------------------------
!
      subroutine cal_psf_ave_rms_data(istep)
!
      use m_psf_results
      use m_norms_4_psf
      use take_avarages_4_psf
!
      integer(kind = kint), intent(in) :: istep
!
!
      call cal_rms_ave_4_psf
      call cal_minmax_psf
!
      write(id_ave_psf,'(i10,1p255E25.15e3)') istep,                    &
               ave_psf(1:ncomptot_psf), area_total_psf
      write(id_rms_psf,'(i10,1p255E25.15e3)') istep,                    &
                rms_psf(1:ncomptot_psf), area_total_psf
!
      end subroutine cal_psf_ave_rms_data
!
!-----------------------------------------------------------------------
!
      end module cal_psf_rms_aves

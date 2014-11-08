!
!      module cal_psf_rms_aves
!
!      Written by H. Matsui on Apr., 2005
!      Modified by H. Matsui on Feb., 2009
!
!      subroutine open_psf_ave_rms_data
!      subroutine s_cal_psf_rms_aves
!
      module cal_psf_rms_aves
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), parameter :: id_ave_psf =  21
      integer(kind = kint), parameter :: id_rms_psf =  22
      integer(kind = kint), parameter :: id_sdev_psf = 25
      integer(kind = kint), parameter :: id_min_psf =  23
      integer(kind = kint), parameter :: id_max_psf =  24
!
      character(len=kchara) :: fname_ave_psf =  'psf_ave.dat'
      character(len=kchara) :: fname_rms_psf =  'psf_rms.dat'
      character(len=kchara) :: fname_sdev_psf = 'psf_sdev.dat'
      character(len=kchara) :: fname_min_psf =  'psf_min.dat'
      character(len=kchara) :: fname_max_psf =  'psf_max.dat'
!
      private :: id_ave_psf,  fname_ave_psf
      private :: id_rms_psf,  fname_rms_psf
      private :: id_sdev_psf, fname_sdev_psf
      private :: id_min_psf,  fname_min_psf
      private :: id_max_psf,  fname_max_psf
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine open_psf_ave_rms_data(file_prefix)
!
      use m_norms_4_psf
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_prefix
      character(len=kchara) :: fname_tmp
!
      write(fname_tmp,'(a9,a)') 'area_ave_', trim(file_prefix)
      call add_dat_extension(fname_tmp, fname_ave_psf)
      call open_psf_int_data(id_ave_psf, fname_ave_psf, ione)
!
      write(fname_tmp,'(a9,a)') 'area_rms_', trim(file_prefix)
      call add_dat_extension(fname_tmp, fname_rms_psf)
      call open_psf_int_data(id_rms_psf, fname_rms_psf, ione)
!
      write(fname_tmp,'(a10,a)') 'area_sdev_', trim(file_prefix)
      call add_dat_extension(fname_tmp, fname_sdev_psf)
      call open_psf_int_data(id_sdev_psf, fname_sdev_psf, ione)
!
      end subroutine open_psf_ave_rms_data
!
!-----------------------------------------------------------------------
!
      subroutine open_psf_range_data(file_prefix)
!
      use m_norms_4_psf
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_prefix
      character(len=kchara) :: fname_tmp
!
!
      write(fname_tmp,'(a9,a)') 'area_min_', trim(file_prefix)
      call add_dat_extension(fname_tmp, fname_min_psf)
      call open_psf_int_data(id_min_psf, fname_min_psf, izero)
!
      write(fname_tmp,'(a9,a)') 'area_max_', trim(file_prefix)
      call add_dat_extension(fname_tmp, fname_max_psf)
      call open_psf_int_data(id_max_psf, fname_max_psf, izero)
!
      end subroutine open_psf_range_data
!
!-----------------------------------------------------------------------
!
      subroutine open_psf_int_data(id_file, file_name, iflag_area)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: id_file, iflag_area
      character(len=kchara), intent(in) :: file_name
!
!
      open(id_file, file=file_name, form='formatted',                   &
     &     status='old',err = 100, position='APPEND')
      return
!
  100 continue
!
      open(id_file, file=file_name, form='formatted')
      call write_headers_psf_int_data(id_file, iflag_area)
!
      end subroutine open_psf_int_data
!
!-----------------------------------------------------------------------
!
      subroutine close_psf_ave_rms_data
!
!
      close(id_ave_psf)
      close(id_rms_psf)
      close(id_sdev_psf)
!
      end subroutine close_psf_ave_rms_data
!
!-----------------------------------------------------------------------
!
      subroutine close_psf_range_data
!
!
      close(id_min_psf)
      close(id_max_psf)
!
      end subroutine close_psf_range_data
!
!-----------------------------------------------------------------------
!
      subroutine write_psf_ave_rms_data(istep, area)
!
      use m_psf_results
      use m_norms_4_psf
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: area
!
!
      write(id_ave_psf,'(i15,1p255E25.15e3)') istep,                    &
               ave_psf(1:ncomptot_psf), area
      write(id_rms_psf,'(i15,1p255E25.15e3)') istep,                    &
                rms_psf(1:ncomptot_psf), area
      write(id_sdev_psf,'(i15,1p255E25.15e3)') istep,                   &
                sdev_psf(1:ncomptot_psf), area
!
      end subroutine write_psf_ave_rms_data
!
!-----------------------------------------------------------------------
!
      subroutine write_psf_range_data(istep)
!
      use m_psf_results
!
      integer(kind = kint), intent(in) :: istep
!
!
      write(id_min_psf,'(i15,1p255E25.15e3)') istep,                    &
     &           xmin_psf(1:ncomptot_psf)
      write(id_max_psf,'(i15,1p255E25.15e3)') istep,                    &
     &           xmax_psf(1:ncomptot_psf)
!
      end subroutine write_psf_range_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_headers_psf_int_data(id_file, iflag_area)
!
      use m_psf_results
!
      integer(kind = kint), intent(in) :: id_file, iflag_area
!
      integer(kind = kint) :: j, k
!
!
      write(id_file,'(a)',advance='no') ' step_no, '
      do j = 1, nfield_psf
        if ( ncomp_psf(j) .eq. 1) then
          write(id_file,'(a,a2)',advance='no')                          &
     &                 trim( psf_data_name(j) ), ', '
        else
          do k = 1, ncomp_psf(j)
            write(id_file,1000,advance='no')                            &
     &                 trim( psf_data_name(j) ), k
          end do
        end if
      end do
!
      if(iflag_area .gt. 0) then
        write(id_file,'(a)') 'area_size, '
      else
        write(id_file,'(a)') ' '
      end if
!
 1000 format(a,'_',i1,', ')
!
      end subroutine write_headers_psf_int_data
!
!-----------------------------------------------------------------------
!
      end module cal_psf_rms_aves

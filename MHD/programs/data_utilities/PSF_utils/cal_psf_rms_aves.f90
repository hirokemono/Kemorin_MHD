!
!      module cal_psf_rms_aves
!
!      Written by H. Matsui on Apr., 2005
!      Modified by H. Matsui on Feb., 2009
!
!      subroutine open_psf_ave_rms_data(file_prefix, psf_phys)
!      subroutine open_psf_range_data(file_prefix, psf_phys)
!      subroutine write_psf_ave_rms_data(istep, area)
!      subroutine write_psf_range_data(istep)
!      subroutine copy_filed_to_phys_data(vector, psf_phys)
!
      module cal_psf_rms_aves
!
      use m_precision
      use m_constants
      use t_phys_data
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
      private :: open_psf_int_data, write_headers_psf_int_data
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine open_psf_ave_rms_data(file_prefix, psf_phys)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_prefix
      type(phys_data), intent(in) :: psf_phys
      character(len=kchara) :: fname_tmp
!
      write(fname_tmp,'(a9,a)') 'area_ave_', trim(file_prefix)
      call add_dat_extension(fname_tmp, fname_ave_psf)
      call open_psf_int_data                                            &
     &   (id_ave_psf, fname_ave_psf, ione, psf_phys)
!
      write(fname_tmp,'(a9,a)') 'area_rms_', trim(file_prefix)
      call add_dat_extension(fname_tmp, fname_rms_psf)
      call open_psf_int_data                                            &
     &   (id_rms_psf, fname_rms_psf, ione, psf_phys)
!
      write(fname_tmp,'(a10,a)') 'area_sdev_', trim(file_prefix)
      call add_dat_extension(fname_tmp, fname_sdev_psf)
      call open_psf_int_data                                            &
     &   (id_sdev_psf, fname_sdev_psf, ione, psf_phys)
!
      end subroutine open_psf_ave_rms_data
!
!-----------------------------------------------------------------------
!
      subroutine open_psf_range_data(file_prefix, psf_phys)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_prefix
      type(phys_data), intent(in) :: psf_phys
      character(len=kchara) :: fname_tmp
!
!
      write(fname_tmp,'(a9,a)') 'area_min_', trim(file_prefix)
      call add_dat_extension(fname_tmp, fname_min_psf)
      call open_psf_int_data                                            &
     &   (id_min_psf, fname_min_psf, izero, psf_phys)
!
      write(fname_tmp,'(a9,a)') 'area_max_', trim(file_prefix)
      call add_dat_extension(fname_tmp, fname_max_psf)
      call open_psf_int_data                                            &
     &   (id_max_psf, fname_max_psf, izero, psf_phys)
!
      end subroutine open_psf_range_data
!
!-----------------------------------------------------------------------
!
      subroutine open_psf_int_data                                      &
     &         (id_file, file_name, iflag_area, psf_phys)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: id_file, iflag_area
      character(len=kchara), intent(in) :: file_name
      type(phys_data), intent(in) :: psf_phys
!
!
      open(id_file, file=file_name, form='formatted',                   &
     &     status='old',err = 100, position='APPEND')
      return
!
  100 continue
!
      open(id_file, file=file_name, form='formatted')
      call write_headers_psf_int_data(id_file, iflag_area, psf_phys)
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
      subroutine write_psf_ave_rms_data(istep, area, psf_aves)
!
      use t_norms_4_psf
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: area
      type(psf_averages), intent(in) :: psf_aves
!
!
      write(id_ave_psf,'(i15,1p255E25.15e3)') istep,                    &
               psf_aves%ave(1:psf_aves%ntot_comp), area
      write(id_rms_psf,'(i15,1p255E25.15e3)') istep,                    &
               psf_aves%rms(1:psf_aves%ntot_comp), area
      write(id_sdev_psf,'(i15,1p255E25.15e3)') istep,                   &
               psf_aves%sdev(1:psf_aves%ntot_comp), area
!
      end subroutine write_psf_ave_rms_data
!
!-----------------------------------------------------------------------
!
      subroutine write_psf_range_data(istep, psf_aves)
!
      use t_norms_4_psf
!
      integer(kind = kint), intent(in) :: istep
      type(psf_averages), intent(in) :: psf_aves
!
!
      write(id_min_psf,'(i15,1p255E25.15e3)') istep,                    &
     &           psf_aves%dmin(1:psf_aves%ntot_comp)
      write(id_max_psf,'(i15,1p255E25.15e3)') istep,                    &
     &           psf_aves%dmax(1:psf_aves%ntot_comp)
!
      end subroutine write_psf_range_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_headers_psf_int_data                             &
     &          (id_file, iflag_area, psf_phys)
!
      integer(kind = kint), intent(in) :: id_file, iflag_area
      type(phys_data), intent(in) :: psf_phys
!
      integer(kind = kint) :: j, k
!
!
      write(id_file,'(a)',advance='no') ' step_no, '
      do j = 1, psf_phys%num_phys
        if (psf_phys%num_component(j) .eq. 1) then
          write(id_file,'(a,a2)',advance='no')                          &
     &                 trim( psf_phys%phys_name(j) ), ', '
        else
          do k = 1, psf_phys%num_component(j)
            write(id_file,1000,advance='no')                            &
     &                 trim( psf_phys%phys_name(j) ), k
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
      subroutine copy_filed_to_phys_data(vector, psf_phys)
!
      use copy_field_smp
!
      type(phys_data), intent(inout) :: psf_phys
      real(kind = kreal), intent(in) ::                                 &
     &           vector(psf_phys%n_point, psf_phys%ntot_phys)
!
!
!$omp parallel
      call copy_all_field_smp(psf_phys%n_point, psf_phys%ntot_phys,     &
     &    vector, psf_phys%d_fld)
!$omp end parallel
!
      end subroutine copy_filed_to_phys_data
!
!-----------------------------------------------------------------------
!
      end module cal_psf_rms_aves

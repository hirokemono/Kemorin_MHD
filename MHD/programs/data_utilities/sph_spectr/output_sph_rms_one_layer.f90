!output_sph_rms_one_layer.f90
!      module output_sph_rms_one_layer
!
!     Written by H. Matsui on Feb., 2008
!
!      subroutine write_sph_1layer_ms_file(my_rank, istep, time)
!      subroutine write_sph_1layer_ms_spec_file(my_rank, istep, time)
!
      module output_sph_rms_one_layer
!
      use m_precision
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_ctl_params_sph_utils
      use m_pickup_sph_spectr_data
!
      implicit none
!
      integer(kind = kint), parameter :: id_file_rms_l =    31
      integer(kind = kint), parameter :: id_file_rms_m =    32
      integer(kind = kint), parameter :: id_file_rms_lm =   33
      integer(kind = kint), parameter :: id_file_rms =      34
!
      private :: id_file_rms
      private :: id_file_rms_l, id_file_rms_m, id_file_rms_lm
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_1layer_ms_file(my_rank, istep, time)
!
      use m_sph_phys_address
      use set_parallel_file_name
      use output_sph_m_square_file
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
!
      character(len = kchara) :: fname_tmp1, fname_tmp2
      character(len = kchara) :: fname_rms, mode_label
      integer(kind = kint) :: kg, k
!
      integer(kind = kint), parameter :: id_file_rms1 =     42
!
!
      if(my_rank .ne. 0) return
!
      call add_int_suffix(id_pick_layer(1), fhead_rms_layer, fname_tmp1)
      call add_int_suffix(id_pick_layer(num_pick_layer), fname_tmp1,    &
     &    fname_tmp2)
      call add_dat_extension(fname_tmp2, fname_rms)
!
      write(mode_label,'(a)') 'radial_id, '
      call open_sph_vol_rms_file(id_file_rms1, fname_rms, mode_label)
!
      do k = 1, num_pick_layer
        kg = id_pick_layer(k)
        write(id_file_rms1,'(i10,1pe23.14e3,i10,1p200e23.14e3)')        &
     &                   istep, time, kg, rms_sph(1:ntot_rms_rj,kg)
      end do
!
      close(id_file_rms1)
!
      end subroutine write_sph_1layer_ms_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_1layer_ms_spec_file(my_rank, istep, time)
!
      use m_sph_phys_address
      use set_parallel_file_name
      use output_sph_m_square_file
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
!
      character(len = kchara) :: fname_tmp1, fname_tmp2
      character(len = kchara) :: fname_rms, mode_label
      integer(kind = kint) :: kg, lm, k
!
!
      if(my_rank .ne. 0) return
!
      write(fname_rms, '(a,a2)') trim(fhead_rms_layer), '_l'
      call add_int_suffix(id_pick_layer(1), fname_rms, fname_tmp1)
      call add_int_suffix(id_pick_layer(num_pick_layer), fname_tmp1,    &
     &    fname_tmp2)
      call add_dat_extension(fname_tmp2, fname_rms)
      write(mode_label,'(a)') 'radial_id    degree'
      call open_sph_vol_rms_file                                        &
     &    (id_file_rms_l, fname_rms, mode_label)
!
      write(fname_rms, '(a,a2)') trim(fhead_rms_layer), '_m'
      call add_int_suffix(id_pick_layer(1), fname_rms, fname_tmp1)
      call add_int_suffix(id_pick_layer(num_pick_layer), fname_tmp1,    &
     &    fname_tmp2)
      call add_dat_extension(fname_tmp2, fname_rms)
      write(mode_label,'(a)') 'radial_id    order'
      call open_sph_vol_rms_file                                        &
     &    (id_file_rms_m, fname_rms, mode_label)
!
      write(fname_rms, '(a,a3)') trim(fhead_rms_layer), '_lm'
      call add_int_suffix(id_pick_layer(1), fname_rms, fname_tmp1)
      call add_int_suffix(id_pick_layer(num_pick_layer), fname_tmp1,    &
     &    fname_tmp2)
      call add_dat_extension(fname_tmp2, fname_rms)
      write(mode_label,'(a)') 'radial_id    diff_deg_order'
      call open_sph_vol_rms_file                                        &
     &    (id_file_rms_lm, fname_rms, mode_label)
!
      do k = 1, num_pick_layer
        kg = id_pick_layer(k)
        do lm = 0, l_truncation
          write(id_file_rms_l,                                          &
     &                  '(i10,1pe23.14e3,2i10,1p200e23.14e3)')          &
     &           istep, time, kg, lm, rms_sph_l(1:ntot_rms_rj,lm,kg)
          write(id_file_rms_m,                                          &
     &                  '(i10,1pe23.14e3,2i10,1p200e23.14e3)')          &
     &           istep, time, kg, lm, rms_sph_m(1:ntot_rms_rj,lm,kg)
          write(id_file_rms_lm,                                         &
     &                  '(i10,1pe23.14e3,2i10,1p200e23.14e3)')          &
     &           istep, time, kg, lm, rms_sph_lm(1:ntot_rms_rj,lm,kg)
        end do
      end do
!
      close(id_file_rms_l)
      close(id_file_rms_m)
      close(id_file_rms_lm)
!
      end subroutine write_sph_1layer_ms_spec_file
!
!  --------------------------------------------------------------------
!
      end module output_sph_rms_one_layer

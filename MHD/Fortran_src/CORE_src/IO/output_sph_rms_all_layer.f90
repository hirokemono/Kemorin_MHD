!
!      module output_sph_rms_all_layer
!
!     Written by H. Matsui on Feb., 2008
!
!      subroutine open_sph_rms_layer_file(my_rank)
!      subroutine close_sph_rms_layer_file(my_rank)
!
!      subroutine write_sph_rms_layer_data(istep, time)
!
!      subroutine read_sph_rms_layerd_data(my_rank, istep, time)
!
      module output_sph_rms_all_layer
!
      use m_precision
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
!
      implicit none
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_rms_layer_file(my_rank)
!
      use set_parallel_file_name
      use output_sph_rms_data
!
      integer(kind = kint), intent(in) :: my_rank
!
      character(len = kchara) :: fname_rms_l,  fname_rms_m
      character(len = kchara) :: fname_rms_lm, fname_rms
!
!
      if(iflag_layer_rms_spec.eq.0 .or. my_rank .ne. 0) return
!
      write(fname_rms_l, '(a,a6)') trim(fhead_rms_layer), '_l.dat'
      write(fname_rms_m, '(a,a6)') trim(fhead_rms_layer), '_m.dat'
      write(fname_rms_lm,'(a,a7)') trim(fhead_rms_layer), '_lm.dat'
      write(fname_rms,   '(a,a4)') trim(fhead_rms_layer), '.dat'
!
      open(id_file_rms_l,    file=fname_rms_l,    form='formatted')
      open(id_file_rms_m,    file=fname_rms_m,    form='formatted')
      open(id_file_rms_lm,   file=fname_rms_lm,   form='formatted')
!
      open(id_file_rms,      file=fname_rms,      form='formatted')
!
      call write_sph_rms_layer_head(id_file_rms, id_file_rms_l,         &
     &    id_file_rms_m, id_file_rms_lm)
!
      end subroutine open_sph_rms_layer_file
!
!  --------------------------------------------------------------------
!
      subroutine close_sph_rms_layer_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(iflag_layer_rms_spec.eq.0 .or. my_rank .ne. 0) return
!
      close(id_file_rms_l)
      close(id_file_rms_m)
      close(id_file_rms_lm)
!
      close(id_file_rms)
!
      end subroutine close_sph_rms_layer_file
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_rms_layer_data(my_rank, istep, time)
!
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
      integer(kind = kint) :: kg, lm
!
!
      if(iflag_layer_rms_spec.eq.0 .or. my_rank .ne. 0) return
!
      do kg = 1, nidx_global_rj(1)
        do lm = 0, l_truncation
          write(id_file_rms_l,'(i10,1pe23.14e3,2i10,1p200e23.14e3)')    &
     &           istep, time, kg, lm, rms_sph_l(1:ntot_rms_rj,lm,kg)
          write(id_file_rms_m,'(i10,1pe23.14e3,2i10,1p200e23.14e3)')    &
     &           istep, time, kg, lm, rms_sph_m(1:ntot_rms_rj,lm,kg)
          write(id_file_rms_lm,'(i10,1pe23.14e3,2i10,1p200e23.14e3)')   &
     &           istep, time, kg, lm, rms_sph_lm(1:ntot_rms_rj,lm,kg)
         end do
      end do
!
      do kg = 1, nidx_global_rj(1)
        write(id_file_rms,'(i10,1pe23.14e3,i10,1p200e23.14e3)')         &
     &                   istep, time, kg, rms_sph(1:ntot_rms_rj,kg)
      end do
!
      end subroutine write_sph_rms_layer_data
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine read_sph_rms_layerd_data(my_rank, istep, time)
!
!
      integer(kind = kint), intent(inout) :: my_rank, istep
      real(kind = kreal), intent(inout) :: time
      integer(kind = kint) :: kg, lm, itmp
!
!
        if (my_rank .eq. 0) then
          do kg = 1, nidx_global_rj(1)
            do lm = 0, l_truncation
              read(id_file_rms_l,*) istep, time, itmp, itmp,            &
     &                rms_sph_l(1:ntot_rms_rj,lm,kg)
              read(id_file_rms_m,*) istep, time, itmp, itmp,            &
     &                rms_sph_m(1:ntot_rms_rj,lm,kg)
              read(id_file_rms_lm,*) istep, time, itmp, itmp,           &
     &                rms_sph_lm(1:ntot_rms_rj,lm,kg)
            end do
          end do
!
          do kg = 1, nidx_global_rj(1)
            read(id_file_rms,*) istep, time, itmp,                      &
     &                          rms_sph(1:ntot_rms_rj,kg)
          end do
!
        end if
!
      end subroutine read_sph_rms_layerd_data
!
!  --------------------------------------------------------------------
!
      end module output_sph_rms_all_layer

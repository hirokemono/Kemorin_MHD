!cal_tave_sph_ene_spectr.f90
!      module cal_tave_sph_ene_spectr
!
!      Written by H. Matsui on May, 2008
!
!      subroutine sum_average_ene_sph
!      subroutine divide_average_ene_sph(icou)
!
!      subroutine write_volt_ave_ene_sph_data
!      subroutine write_average_ene_sph_data
!
!      subroutine count_degree_on_layer_data
!      subroutine count_degree_one_layer_data
!      subroutine count_degree_on_volume_data
!
!      subroutine read_org_layer_ene_data(istep, ierr)
!      subroutine read_org_volume_ene_data(istep, ierr)
!
      module cal_tave_sph_ene_spectr
!
      use m_precision
      use m_constants
      use m_tave_sph_ene_spectr
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sum_average_ene_sph
!
      integer(kind = kint) :: kr, nd, lth
!
!$omp parallel private(kr,lth,nd)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp_sph_tave
          ave_spec_t(nd,kr) = ave_spec_t(nd,kr) + spectr_t(nd,kr)
        end do
!$omp end do nowait
!
        do lth = 0, ltr_sph
!$omp do
          do nd = 1, ncomp_sph_tave
            ave_spec_l(nd,lth,kr) =  ave_spec_l(nd,lth,kr)              &
     &                              + spectr_l(nd,lth,kr)
            ave_spec_m(nd,lth,kr) =  ave_spec_m(nd,lth,kr)              &
     &                              + spectr_m(nd,lth,kr)
            ave_spec_lm(nd,lth,kr) = ave_spec_lm(nd,lth,kr)             &
     &                              + spectr_lm(nd,lth,kr)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine sum_average_ene_sph
!
!   --------------------------------------------------------------------
!
      subroutine divide_average_ene_sph(icou)
!
      integer(kind = kint), intent(in) :: icou
      integer(kind = kint) :: kr, nd, lth
!
!
!$omp parallel private(kr,lth,nd)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp_sph_tave
          ave_spec_t(nd,kr) = ave_spec_t(nd,kr) / dble(icou)
        end do
!$omp end do nowait
!
        do lth = 0, ltr_sph
!$omp do
          do nd = 1, ncomp_sph_tave
            ave_spec_l(nd,lth,kr) =  ave_spec_l(nd,lth,kr) / dble(icou)
            ave_spec_m(nd,lth,kr) =  ave_spec_m(nd,lth,kr) / dble(icou)
            ave_spec_lm(nd,lth,kr) = ave_spec_lm(nd,lth,kr)             &
     &                                 / dble(icou)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine divide_average_ene_sph
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_volt_ave_ene_sph_data
!
      integer(kind = kint) :: kr, lth
!
!
      do kr = 1, nri_sph
        write(id_file_rms,'(i10,1pE25.15e3,i10,1p255E25.15e3)')         &
     &         ied_true, time_sph, izero,                               &
     &         ave_spec_t(1:ncomp_sph_tave,kr)
!
        do lth = 0, ltr_sph
          write(id_file_rms_l,'(i10,1pE25.15e3,i10,1p255E25.15e3)')     &
     &         ied_true, time_sph, lth,                                 &
     &         ave_spec_l(1:ncomp_sph_tave,lth,kr)
          write(id_file_rms_m,'(i10,1pE25.15e3,i10,1p255E25.15e3)')     &
     &         ied_true, time_sph, lth,                                 &
     &         ave_spec_m(1:ncomp_sph_tave,lth,kr)
          write(id_file_rms_lm,'(i10,1pE25.15e3,i10,1p255E25.15e3)')    &
     &         ied_true, time_sph, lth,                                 &
     &         ave_spec_lm(1:ncomp_sph_tave,lth,kr)
        end do
      end do
!
      end subroutine write_volt_ave_ene_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine write_average_ene_sph_data
!
      integer(kind = kint) :: kr, lth
!
!
      do kr = 1, nri_sph
        write(id_file_rms,'(i10,1pE25.15e3,2i10,1p255E25.15e3)')        &
     &         ied_true, time_sph, kr_sph(kr), izero,                   &
     &         ave_spec_t(1:ncomp_sph_tave,kr)
!
        do lth = 0, ltr_sph
          write(id_file_rms_l,'(i10,1pE25.15e3,2i10,1p255E25.15e3)')    &
     &         ied_true, time_sph, kr_sph(kr), lth,                     &
     &         ave_spec_l(1:ncomp_sph_tave,lth,kr)
          write(id_file_rms_m,'(i10,1pE25.15e3,2i10,1p255E25.15e3)')    &
     &         ied_true, time_sph, kr_sph(kr), lth,                     &
     &         ave_spec_m(1:ncomp_sph_tave,lth,kr)
          write(id_file_rms_lm,'(i10,1pE25.15e3,2i10,1p255E25.15e3)')   &
     &         ied_true, time_sph, kr_sph(kr), lth,                     &
     &         ave_spec_lm(1:ncomp_sph_tave,lth,kr)
        end do
      end do
!
      end subroutine write_average_ene_sph_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_degree_on_layer_data
!
      use skip_comment_f
!
      character(len=255) :: character_4_read
      integer(kind = kint) :: num, itmp, nfld, i
!
!
!
      open (id_file_rms_l,file=fname_org_rms_l)
!
      call skip_comment(character_4_read, id_file_rms_l)
      read(id_file_rms_l,*) nri_sph, ltr_sph
      write(*,*) 'ltr_sph', ltr_sph
      write(*,*) 'nri_sph', nri_sph
      call skip_comment(character_4_read, id_file_rms_l)
      call skip_comment(character_4_read, id_file_rms_l)
      call skip_comment(character_4_read, id_file_rms_l)
      read(id_file_rms_l,*) nfld, ncomp_sph_tave
!
      num_time_labels = 4
      write(*,*) 'ncomp_sph_tave', ncomp_sph_tave
      call allocate_tave_sph_espec_name
!
!
      read(id_file_rms_l,*) (itmp,i=1,nfld)
!
      num = size(ene_sph_spec_name)
      write(*,*) 'num vector', num
      read(id_file_rms_l,*)  ene_sph_spec_name(1:num)
      do i = 1, NUM
        write(*,*) i, trim(ene_sph_spec_name(i))
      end  do
!
      close(id_file_rms_l)
!
      end subroutine count_degree_on_layer_data
!
!   --------------------------------------------------------------------
!
      subroutine count_degree_one_layer_data
!
      use skip_comment_f
!
      character(len=255) :: character_4_read
      integer(kind = kint) :: num, itmp, nfld, i
!
!
!
      open (id_file_rms_l,file=fname_org_rms_l)
!
      call skip_comment(character_4_read, id_file_rms_l)
      read(id_file_rms_l,*) nri_sph, ltr_sph
      nri_sph = 1
      write(*,*) 'ltr_sph', ltr_sph
      write(*,*) 'nri_sph', nri_sph
      call skip_comment(character_4_read, id_file_rms_l)
      call skip_comment(character_4_read, id_file_rms_l)
      call skip_comment(character_4_read, id_file_rms_l)
      read(id_file_rms_l,*) nfld, ncomp_sph_tave
!
      num_time_labels = 4
      write(*,*) 'ncomp_sph_tave', ncomp_sph_tave
      call allocate_tave_sph_espec_name
!
!
      read(id_file_rms_l,*) (itmp,i=1,nfld)
!
      num = size(ene_sph_spec_name)
      write(*,*) 'num vector', num
      read(id_file_rms_l,*)  ene_sph_spec_name(1:num)
      do i = 1, NUM
        write(*,*) i, trim(ene_sph_spec_name(i))
      end  do
!
      close(id_file_rms_l)
!
      end subroutine count_degree_one_layer_data
!
!   --------------------------------------------------------------------
!
      subroutine count_degree_on_volume_data
!
      use skip_comment_f
!
      character(len=255) :: character_4_read
      integer(kind = kint) :: num, itmp, nfld, i
!
!
      open (id_file_rms_l,file=fname_org_rms_l)
!
      call skip_comment(character_4_read, id_file_rms_l)
      read(id_file_rms_l,*) itmp, ltr_sph
      write(*,*) 'ltr_sph', ltr_sph
      call skip_comment(character_4_read, id_file_rms_l)
      call skip_comment(character_4_read, id_file_rms_l)
      call skip_comment(character_4_read, id_file_rms_l)
      read(id_file_rms_l,*) nfld, ncomp_sph_tave
!
      num_time_labels = 3
      nri_sph = 1
      write(*,*) 'ncomp_sph_tave', ncomp_sph_tave
      write(*,*) 'nri_sph', nri_sph
      call allocate_tave_sph_espec_name
!
!
      read(id_file_rms_l,*) (itmp,i=1,nfld)
!
      num = size(ene_sph_spec_name)
      write(*,*) 'num v', num
      read(id_file_rms_l,*)  ene_sph_spec_name(1:num)
      DO I = 1, NUM
        write(*,*) i, trim(ene_sph_spec_name(i))
      end  do
!
      close(id_file_rms_l)
!
      end subroutine count_degree_on_volume_data
!
!   --------------------------------------------------------------------
!
      subroutine read_org_layer_ene_data(istep, ierr)
!
      integer(kind = kint), intent(inout) :: istep, ierr
      integer(kind = kint) :: itmp
      integer(kind = kint) :: kr, lth
!
!
      ierr = 0
      do kr = 1, nri_sph
        read(id_file_rms,*,err=99,end=99) istep, time_sph,              &
     &         kr_sph(kr), spectr_t(1:ncomp_sph_tave,kr)
        do lth = 0, ltr_sph
          read(id_file_rms_l,*,err=99,end=99) istep, time_sph,          &
     &         itmp, itmp, spectr_l(1:ncomp_sph_tave,lth,kr)
          read(id_file_rms_m,*,err=99,end=99) istep, time_sph,          &
     &         itmp, itmp, spectr_m(1:ncomp_sph_tave,lth,kr)
          read(id_file_rms_lm,*,err=99,end=99) istep, time_sph,         &
     &         itmp, itmp, spectr_lm(1:ncomp_sph_tave,lth,kr)
        end do
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_org_layer_ene_data
!
!   --------------------------------------------------------------------
!
      subroutine read_org_volume_ene_data(istep, ierr)
!
      integer(kind = kint), intent(inout) :: istep, ierr
      integer(kind = kint) :: itmp
      integer(kind = kint) :: lth
!
!
      ierr = 0
      read(id_file_rms,*,err=99,end=99) istep, time_sph,                &
     &         spectr_t(1:ncomp_sph_tave,ione)
      do lth = 0, ltr_sph
          read(id_file_rms_l,*,err=99,end=99) istep, time_sph,          &
     &         itmp, spectr_l(1:ncomp_sph_tave,lth,ione)
          read(id_file_rms_m,*,err=99,end=99) istep, time_sph,          &
     &         itmp, spectr_m(1:ncomp_sph_tave,lth,ione)
          read(id_file_rms_lm,*,err=99,end=99) istep, time_sph,         &
     &         itmp, spectr_lm(1:ncomp_sph_tave,lth,ione)
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_org_volume_ene_data
!
!   --------------------------------------------------------------------
!
      end module cal_tave_sph_ene_spectr

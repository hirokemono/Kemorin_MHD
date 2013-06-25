!
      program add_elegrp_sph
!
      use m_precision
!
      use m_parallel_var_dof
      use analyzer_add_ele_group
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialize_add_egrp
!
      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program add_elegrp_sph
